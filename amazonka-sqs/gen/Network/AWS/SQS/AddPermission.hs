{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.AddPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a permission to a queue for a specific <http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> . This allows sharing access to the queue.
--
--
-- When you create a queue, you have full control access rights for the queue. Only you, the owner of the queue, can grant or deny permissions to the queue. For more information about these permissions, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html Shared Queues> in the /Amazon Simple Queue Service Developer Guide/ .
--
module Network.AWS.SQS.AddPermission
    (
    -- * Creating a Request
      addPermission
    , AddPermission
    -- * Request Lenses
    , apQueueURL
    , apLabel
    , apAWSAccountIds
    , apActions

    -- * Destructuring the Response
    , addPermissionResponse
    , AddPermissionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SQS.Types
import Network.AWS.SQS.Types.Product

-- |
--
--
--
-- /See:/ 'addPermission' smart constructor.
data AddPermission = AddPermission'
  { _apQueueURL      :: !Text
  , _apLabel         :: !Text
  , _apAWSAccountIds :: ![Text]
  , _apActions       :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'apQueueURL' - The URL of the Amazon SQS queue to which permissions are added. Queue URLs are case-sensitive.
--
-- * 'apLabel' - The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
--
-- * 'apAWSAccountIds' - The AWS account number of the <http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AWSCredentials.html Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
--
-- * 'apActions' - The action the client wants to allow for the specified principal. The following values are valid:     * @*@      * @ChangeMessageVisibility@      * @DeleteMessage@      * @GetQueueAttributes@      * @GetQueueUrl@      * @ReceiveMessage@      * @SendMessage@  For more information about these actions, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html#PermissionTypes Understanding Permissions> in the /Amazon Simple Queue Service Developer Guide/ . Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
addPermission
    :: Text -- ^ 'apQueueURL'
    -> Text -- ^ 'apLabel'
    -> AddPermission
addPermission pQueueURL_ pLabel_ =
  AddPermission'
    { _apQueueURL = pQueueURL_
    , _apLabel = pLabel_
    , _apAWSAccountIds = mempty
    , _apActions = mempty
    }


-- | The URL of the Amazon SQS queue to which permissions are added. Queue URLs are case-sensitive.
apQueueURL :: Lens' AddPermission Text
apQueueURL = lens _apQueueURL (\ s a -> s{_apQueueURL = a})

-- | The unique identification of the permission you're setting (for example, @AliceSendMessage@ ). Maximum 80 characters. Allowed characters include alphanumeric characters, hyphens (@-@ ), and underscores (@_@ ).
apLabel :: Lens' AddPermission Text
apLabel = lens _apLabel (\ s a -> s{_apLabel = a})

-- | The AWS account number of the <http://docs.aws.amazon.com/general/latest/gr/glos-chap.html#P principal> who is given permission. The principal must have an AWS account, but does not need to be signed up for Amazon SQS. For information about locating the AWS account identification, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/AWSCredentials.html Your AWS Identifiers> in the /Amazon Simple Queue Service Developer Guide/ .
apAWSAccountIds :: Lens' AddPermission [Text]
apAWSAccountIds = lens _apAWSAccountIds (\ s a -> s{_apAWSAccountIds = a}) . _Coerce

-- | The action the client wants to allow for the specified principal. The following values are valid:     * @*@      * @ChangeMessageVisibility@      * @DeleteMessage@      * @GetQueueAttributes@      * @GetQueueUrl@      * @ReceiveMessage@      * @SendMessage@  For more information about these actions, see <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/SQSDeveloperGuide/acp-overview.html#PermissionTypes Understanding Permissions> in the /Amazon Simple Queue Service Developer Guide/ . Specifying @SendMessage@ , @DeleteMessage@ , or @ChangeMessageVisibility@ for @ActionName.n@ also grants permissions for the corresponding batch versions of those actions: @SendMessageBatch@ , @DeleteMessageBatch@ , and @ChangeMessageVisibilityBatch@ .
apActions :: Lens' AddPermission [Text]
apActions = lens _apActions (\ s a -> s{_apActions = a}) . _Coerce

instance AWSRequest AddPermission where
        type Rs AddPermission = AddPermissionResponse
        request = postQuery sqs
        response = receiveNull AddPermissionResponse'

instance Hashable AddPermission where

instance NFData AddPermission where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
--
addPermissionResponse
    :: AddPermissionResponse
addPermissionResponse = AddPermissionResponse'


instance NFData AddPermissionResponse where
