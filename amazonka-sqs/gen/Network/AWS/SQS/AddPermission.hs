{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds a permission to a queue for a specific
-- <http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>.
-- This allows for sharing access to the queue.
--
-- When you create a queue, you have full control access rights for the
-- queue. Only you (as owner of the queue) can grant or deny permissions to
-- the queue. For more information about these permissions, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html Shared Queues>
-- in the /Amazon SQS Developer Guide/.
--
-- @AddPermission@ writes an Amazon SQS-generated policy. If you want to
-- write your own policy, use SetQueueAttributes to upload your policy. For
-- more information about writing your own policy, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AccessPolicyLanguage.html Using The Access Policy Language>
-- in the /Amazon SQS Developer Guide/.
--
-- Some API actions take lists of parameters. These lists are specified
-- using the @param.n@ notation. Values of @n@ are integers starting from
-- 1. For example, a parameter list with two elements looks like this:
--
-- @&Attribute.1=this@
--
-- @&Attribute.2=that@
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_AddPermission.html>
module Network.AWS.SQS.AddPermission
    (
    -- * Request
      AddPermission
    -- ** Request constructor
    , addPermission
    -- ** Request lenses
    , apQueueURL
    , apLabel
    , apAWSAccountIds
    , apActions

    -- * Response
    , AddPermissionResponse
    -- ** Response constructor
    , addPermissionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'addPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'apQueueURL'
--
-- * 'apLabel'
--
-- * 'apAWSAccountIds'
--
-- * 'apActions'
data AddPermission = AddPermission'
    { _apQueueURL      :: !Text
    , _apLabel         :: !Text
    , _apAWSAccountIds :: ![Text]
    , _apActions       :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'AddPermission' smart constructor.
addPermission :: Text -> Text -> AddPermission
addPermission pQueueURL pLabel =
    AddPermission'
    { _apQueueURL = pQueueURL
    , _apLabel = pLabel
    , _apAWSAccountIds = mempty
    , _apActions = mempty
    }

-- | The URL of the Amazon SQS queue to take action on.
apQueueURL :: Lens' AddPermission Text
apQueueURL = lens _apQueueURL (\ s a -> s{_apQueueURL = a});

-- | The unique identification of the permission you\'re setting (e.g.,
-- @AliceSendMessage@). Constraints: Maximum 80 characters; alphanumeric
-- characters, hyphens (-), and underscores (_) are allowed.
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\ s a -> s{_apLabel = a});

-- | The AWS account number of the
-- <http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal>
-- who will be given permission. The principal must have an AWS account,
-- but does not need to be signed up for Amazon SQS. For information about
-- locating the AWS account identification, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AWSCredentials.html Your AWS Identifiers>
-- in the /Amazon SQS Developer Guide/.
apAWSAccountIds :: Lens' AddPermission [Text]
apAWSAccountIds = lens _apAWSAccountIds (\ s a -> s{_apAWSAccountIds = a});

-- | The action the client wants to allow for the specified principal. The
-- following are valid values:
-- @* | SendMessage | ReceiveMessage | DeleteMessage | ChangeMessageVisibility | GetQueueAttributes | GetQueueUrl@.
-- For more information about these actions, see
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html#PermissionTypes Understanding Permissions>
-- in the /Amazon SQS Developer Guide/.
--
-- Specifying @SendMessage@, @DeleteMessage@, or @ChangeMessageVisibility@
-- for the @ActionName.n@ also grants permissions for the corresponding
-- batch versions of those actions: @SendMessageBatch@,
-- @DeleteMessageBatch@, and @ChangeMessageVisibilityBatch@.
apActions :: Lens' AddPermission [Text]
apActions = lens _apActions (\ s a -> s{_apActions = a});

instance AWSRequest AddPermission where
        type Sv AddPermission = SQS
        type Rs AddPermission = AddPermissionResponse
        request = post
        response = receiveNull AddPermissionResponse'

instance ToHeaders AddPermission where
        toHeaders = const mempty

instance ToPath AddPermission where
        toPath = const "/"

instance ToQuery AddPermission where
        toQuery AddPermission'{..}
          = mconcat
              ["Action" =: ("AddPermission" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _apQueueURL, "Label" =: _apLabel,
               toQueryList "AWSAccountId" _apAWSAccountIds,
               toQueryList "ActionName" _apActions]

-- | /See:/ 'addPermissionResponse' smart constructor.
data AddPermissionResponse =
    AddPermissionResponse'
    deriving (Eq,Read,Show)

-- | 'AddPermissionResponse' smart constructor.
addPermissionResponse :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse'
