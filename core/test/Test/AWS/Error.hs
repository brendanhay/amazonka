{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Module      : Test.AWS.Error
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Error (tests) where

import qualified Data.ByteString.Char8    as BS8
import qualified Data.Foldable            as Fold
import           Data.List                (sort)
import           Data.Monoid
import           Data.String
import qualified Data.Text                as Text
import qualified Data.Text.Encoding       as Text
import           Network.AWS.Error
import           Network.AWS.Prelude
import           Test.AWS.Arbitrary       ()
import           Test.QuickCheck.Property ()
import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "errors"
    [ testGroup "xml"
        [ testCase "ec2" $ xmlError ec2
            "VolumeInUse"
            "vol-8c8cea98 is already attached to an instance"
            "c0ca7700-c515-4653-87e3-f1a9ce6416e8"

        , testCase "route53" $ xmlError route53
            "InvalidChangeBatch"
            "Tried to delete resource record set noexist.example.com. type A,but it was not found"
            "default_rid"

        , testCase "sqs" $ xmlError sqs
            "InvalidParameterValue"
            "Value (quename_nonalpha) for parameter QueueName is invalid. Must be an alphanumeric String of 1 to 80 in length"
            "42d59b56-7407-4c4a-be0f-4c88daeea257"
        ]
    ]

xmlError :: LazyByteString
         -> ErrorCode
         -> ErrorMessage
         -> RequestId
         -> Assertion
xmlError bs c m r = actual @?= Right expect
  where
    expect = serviceError a s h (Just c) (Just m) (Just r)
    actual =
        case parseXMLError a s h bs of
            ServiceError e -> Right e
            e              -> Left $ "unexpected error: " ++ show e

    a = "Test"
    s = toEnum 400
    h = [(hAMZRequestId, "default_rid")]

-- Samples representative of differing xml errors.

ec2 :: LazyByteString
ec2 = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Response><Errors><Error><Code>VolumeInUse</Code><Message>vol-8c8cea98 is already attached to an instance</Message></Error></Errors><RequestID>c0ca7700-c515-4653-87e3-f1a9ce6416e8</RequestID></Response>"

route53 :: LazyByteString
route53 = "<InvalidChangeBatch xmlns=\"https://route53.amazonaws.com/doc/2013-04-01/\"><Messages><Message>Tried to delete resource record set noexist.example.com. type A,but it was not found</Message></Messages></InvalidChangeBatch>"

sqs :: LazyByteString
sqs = "<ErrorResponse><Error><Type>Sender</Type><Code>InvalidParameterValue</Code><Message>Value (quename_nonalpha) for parameter QueueName is invalid. Must be an alphanumeric String of 1 to 80 in length</Message></Error><RequestId>42d59b56-7407-4c4a-be0f-4c88daeea257</RequestId></ErrorResponse>"
