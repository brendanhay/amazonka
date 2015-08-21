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
import           Data.Text           (Text)
import           Network.AWS.SQS
import           Test.AWS.Gen.SQS
import           Test.Tasty

tests :: [TestTree]
tests = []

fixtures :: [TestTree]
fixtures =
    [ testGroup "request" $
        [ testSendMessage $
            sendMessage url "This+is+a+test+message"
                & smMessageAttributes .~ Map.fromList
                    [ ("test_attribute_name_1", messageAttributeValue "String"
                        & mavStringValue ?~ "test_attribute_value_1")
                    , ("test_attribute_name_2", messageAttributeValue "String"
                        & mavStringValue ?~ "test_attribute_value_2")
                    ]

        , testChangeMessageVisibility $
            changeMessageVisibility url handle 60

        , testChangeMessageVisibilityBatch $
            changeMessageVisibilityBatch url
               & cmvbEntries .~
                   [ changeMessageVisibilityBatchRequestEntry
                       "change_visibility_msg_2" handle
                           & cVisibilityTimeout ?~ 45
                   , changeMessageVisibilityBatchRequestEntry
                       "change_visibility_msg_3" handle
                           & cVisibilityTimeout ?~ 45
                   ]
        ]

    , testGroup "response"
        [ testGetQueueURLResponse $ getQueueURLResponse 200 url
        , testPurgeQueueResponse  $ purgeQueueResponse

        , testSendMessageResponse $
            sendMessageResponse 200
                & smrsMessageId              ?~ "5fea7756-0ea4-451a-a703-a558b933e274"
                & smrsMD5OfMessageBody       ?~ "fafb00f5732ab283681e124bf8747ed1"
                & smrsMD5OfMessageAttributes ?~ "3ae8f24a165a8cedc005670c81a27295"

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
        ]
    ]

url :: Text
url = "http://sqs.us-east-1.amazonaws.com/123456789012/testQueue/"

handle :: Text
handle = "MbZli+JvwwJaBV+3dcjk2W2vA3+STFFljTJg6HYSasuWXPJB/WeR4mq21A2x/KSbkJ0="
