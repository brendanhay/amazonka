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
-- Module      : Network.AWS.Glacier.GetVaultNotifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @notification-configuration@ subresource of the specified vault.
--
--
-- For information about setting a notification configuration on a vault, see 'SetVaultNotifications' . If a notification configuration for a vault is not set, the operation returns a @404 Not Found@ error. For more information about vault notifications, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier> .
--
-- An AWS account has full permission to perform all operations (actions). However, AWS Identity and Access Management (IAM) users don't have any permissions by default. You must grant them explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)> .
--
-- For conceptual information and underlying REST API, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html Get Vault Notification Configuration > in the /Amazon Glacier Developer Guide/ .
--
module Network.AWS.Glacier.GetVaultNotifications
    (
    -- * Creating a Request
      getVaultNotifications
    , GetVaultNotifications
    -- * Request Lenses
    , gvnAccountId
    , gvnVaultName

    -- * Destructuring the Response
    , getVaultNotificationsResponse
    , GetVaultNotificationsResponse
    -- * Response Lenses
    , gvnrsVaultNotificationConfig
    , gvnrsResponseStatus
    ) where

import Network.AWS.Glacier.Types
import Network.AWS.Glacier.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Provides options for retrieving the notification configuration set on an Amazon Glacier vault.
--
--
--
-- /See:/ 'getVaultNotifications' smart constructor.
data GetVaultNotifications = GetVaultNotifications'
  { _gvnAccountId :: !Text
  , _gvnVaultName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVaultNotifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvnAccountId' - The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
--
-- * 'gvnVaultName' - The name of the vault.
getVaultNotifications
    :: Text -- ^ 'gvnAccountId'
    -> Text -- ^ 'gvnVaultName'
    -> GetVaultNotifications
getVaultNotifications pAccountId_ pVaultName_ =
  GetVaultNotifications'
    {_gvnAccountId = pAccountId_, _gvnVaultName = pVaultName_}


-- | The @AccountId@ value is the AWS account ID of the account that owns the vault. You can either specify an AWS account ID or optionally a single '@-@ ' (hyphen), in which case Amazon Glacier uses the AWS account ID associated with the credentials used to sign the request. If you use an account ID, do not include any hyphens ('-') in the ID.
gvnAccountId :: Lens' GetVaultNotifications Text
gvnAccountId = lens _gvnAccountId (\ s a -> s{_gvnAccountId = a})

-- | The name of the vault.
gvnVaultName :: Lens' GetVaultNotifications Text
gvnVaultName = lens _gvnVaultName (\ s a -> s{_gvnVaultName = a})

instance AWSRequest GetVaultNotifications where
        type Rs GetVaultNotifications =
             GetVaultNotificationsResponse
        request = get glacier
        response
          = receiveJSON
              (\ s h x ->
                 GetVaultNotificationsResponse' <$>
                   (eitherParseJSON x) <*> (pure (fromEnum s)))

instance Hashable GetVaultNotifications where

instance NFData GetVaultNotifications where

instance ToHeaders GetVaultNotifications where
        toHeaders = const mempty

instance ToPath GetVaultNotifications where
        toPath GetVaultNotifications'{..}
          = mconcat
              ["/", toBS _gvnAccountId, "/vaults/",
               toBS _gvnVaultName, "/notification-configuration"]

instance ToQuery GetVaultNotifications where
        toQuery = const mempty

-- | Contains the Amazon Glacier response to your request.
--
--
--
-- /See:/ 'getVaultNotificationsResponse' smart constructor.
data GetVaultNotificationsResponse = GetVaultNotificationsResponse'
  { _gvnrsVaultNotificationConfig :: !(Maybe VaultNotificationConfig)
  , _gvnrsResponseStatus          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetVaultNotificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvnrsVaultNotificationConfig' - Returns the notification configuration set on the vault.
--
-- * 'gvnrsResponseStatus' - -- | The response status code.
getVaultNotificationsResponse
    :: Int -- ^ 'gvnrsResponseStatus'
    -> GetVaultNotificationsResponse
getVaultNotificationsResponse pResponseStatus_ =
  GetVaultNotificationsResponse'
    { _gvnrsVaultNotificationConfig = Nothing
    , _gvnrsResponseStatus = pResponseStatus_
    }


-- | Returns the notification configuration set on the vault.
gvnrsVaultNotificationConfig :: Lens' GetVaultNotificationsResponse (Maybe VaultNotificationConfig)
gvnrsVaultNotificationConfig = lens _gvnrsVaultNotificationConfig (\ s a -> s{_gvnrsVaultNotificationConfig = a})

-- | -- | The response status code.
gvnrsResponseStatus :: Lens' GetVaultNotificationsResponse Int
gvnrsResponseStatus = lens _gvnrsResponseStatus (\ s a -> s{_gvnrsResponseStatus = a})

instance NFData GetVaultNotificationsResponse where
