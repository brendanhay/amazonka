{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.AWS.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.SQS
    ( tests
    , fixtures
    ) where

import           Control.Lens
import qualified Data.HashMap.Strict as Map
import           Network.AWS.SQS
import           Test.AWS.Gen.SQS
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "request" $
        [ testSendMessage $
            sendMessage "http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/"
                        "This+is+a+test+message"
                & smMessageAttributes .~ Map.fromList
                    [ ("test_attribute_name_1", messageAttributeValue "String"
                        & mavStringValue ?~ "test_attribute_value_1")
                    , ("test_attribute_name_2", messageAttributeValue "String"
                        & mavStringValue ?~ "test_attribute_value_2")
                    ]
        ]

    , testGroup "response"
        [ testGetQueueURLResponse $
            getQueueURLResponse 200 "http://us-east-1.amazonaws.com/123456789012/testQueue"

        , testPurgeQueueResponse $
            purgeQueueResponse

        -- FIXME: waiting on response to https://github.com/boto/botocore/issues/602
        -- , testReceiveMessageResponse $
        --     receiveMessageResponse 200 & rmrMessages .~ message
        --         & mesMessageId     ?~ "5fea7756-0ea4-451a-a703-a558b933e274"
        --         & mesReceiptHandle ?~ "MbZj6wDWli+JvwwJaBV+3dcjk2YW2vA3+STFFljTM8tJJg6HRG6PYSasuWXPJB+CwLj1FjgXUv1uSj1gUPAWV66FU/WeR4mq2OKpEGYWbnLmpRCJVAyeMjeU5ZBdtcQ+QEauMZc8ZRv37sIW2iJKq3M9MFx1YvV11A2x/KSbkJ0="
        --         & mesMD5OfBody     ?~ "fafb00f5732ab283681e124bf8747ed1"
        --         & mesBody          ?~ "This is a test message"
        --         & mesAttributes    .~ Map.fromList
        --             [ (SenderId,                         195004372649)
        --             , (SentTimestamp,                    1238099229000)
        --             , (ApproximateReceiveCount,          5)
        --             , (ApproximateFirstReceiveTimestamp, 1250700979248)
        --             ]

        , testSendMessageResponse $
            sendMessageResponse 200
        ]
    ]
