{-# LANGUAGE OverloadedStrings #-}

-- Module      : Test.AWS.SQS
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.SQS
     ( tests
     , fixtures
     ) where

import           Network.AWS.SQS
import           Network.HTTP.Types.Status
import           Test.AWS.Gen.SQS
import           Test.Tasty

tests :: TestTree
tests = testGroup "SQS"
    [
    ]

fixtures :: TestTree
fixtures = testGroup "SQS"
    [ testGroup "response"
        [ getQueueURLResponseTest $
            getQueueURLResponse "url" status200

        , purgeQueueResponseTest $
            purgeQueueResponse

        -- , sendMessageResponseTest $
        --     sendMessageResponse

        -- , changeMessageVisibilityBatchResponseTest $
        --     changeMessageVisibilityBatchResponse

        -- , removePermissionResponseTest $
        --     removePermissionResponse

        -- , getQueueAttributesResponseTest $
        --     getQueueAttributesResponse

        -- , listQueuesResponseTest $
        --     listQueuesResponse

        -- , receiveMessageResponseTest $
        --     receiveMessageResponse

        -- , deleteQueueResponseTest $
        --     deleteQueueResponse

        -- , deleteMessageBatchResponseTest $
        --     deleteMessageBatchResponse

        -- , setQueueAttributesResponseTest $
        --     setQueueAttributesResponse

        -- , listDeadLetterSourceQueuesResponseTest $
        --     listDeadLetterSourceQueuesResponse

        -- , addPermissionResponseTest $
        --     addPermissionResponse

        -- , deleteMessageResponseTest $
        --     deleteMessageResponse

        -- , createQueueResponseTest $
        --     createQueueResponse

        -- , changeMessageVisibilityResponseTest $
        --     changeMessageVisibilityResponse

        -- , sendMessageBatchResponseTest $
        --     sendMessageBatchResponse
        ]
    ]
