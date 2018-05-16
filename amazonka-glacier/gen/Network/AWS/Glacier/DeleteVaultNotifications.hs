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
-- Module      : Network.AWS.Glacier.DeleteVaultNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the notification configuration set for a vault. The operation is eventually consistent; that is, it might take some time for Amazon Glacier to completely disable the notifications and you might still receive some notifications for a short time after you send the delete request.
--
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html Delete Vault Notification Configuration > in the Amazon Glacier Developer Guide.
--
module Network.AWS.Glacier.DeleteVaultNotifications
    (
    -- * Creating a Request
      deleteVaultNotifications
    , DeleteVaultNotifications
    -- * Request Lenses
    , dvnAccountId
    , dvnVaultName

    -- * Destructuring the Response
    , deleteVaultNotificationsResponse
    , DeleteVaultNotificationsResponse
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for deleting a vault notification configuration from an Amazon Glacier vault.
--
--
--
-- /See:/ 'deleteVaultNotifications' smart constructor.
data DeleteVaultNotifications = DeleteVaultNotifications'
  { _dvnAccountId :: !Text
  , _dvnVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVaultNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvnAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'dvnVaultName' - The name of the vault.
deleteVaultNotifications
    :: Text -- ^ 'dvnAccountId'
    -> Text -- ^ 'dvnVaultName'
    -> DeleteVaultNotifications
deleteVaultNotifications pAccountId_ pVaultName_ =
  DeleteVaultNotifications'
    {_dvnAccountId = pAccountId_, _dvnVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
dvnAccountId :: Lens' DeleteVaultNotifications Text
dvnAccountId = lens _dvnAccountId (\ s a -> s{_dvnAccountId = a})

-- | The name of the vault.
dvnVaultName :: Lens' DeleteVaultNotifications Text
dvnVaultName = lens _dvnVaultName (\ s a -> s{_dvnVaultName = a})

instance AWSRequest DeleteVaultNotifications where
        type Rs DeleteVaultNotifications =
             DeleteVaultNotificationsResponse
        request = delete glacier
        response
          = receiveNull DeleteVaultNotificationsResponse'

instance Hashable DeleteVaultNotifications where

instance NFData DeleteVaultNotifications where

instance ToHeaders DeleteVaultNotifications where
        toHeaders = const mempty

instance ToPath DeleteVaultNotifications where
        toPath DeleteVaultNotifications'{..}
          = mconcat
              ["/", toBS _dvnAccountId, "/vaults/",
               toBS _dvnVaultName, "/notification-configuration"]

instance ToQuery DeleteVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'deleteVaultNotificationsResponse' smart constructor.
data DeleteVaultNotificationsResponse =
  DeleteVaultNotificationsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVaultNotificationsResponse' with the minimum fields required to make a request.
--
deleteVaultNotificationsResponse
    :: DeleteVaultNotificationsResponse
deleteVaultNotificationsResponse = DeleteVaultNotificationsResponse'


instance NFData DeleteVaultNotificationsResponse
         where
