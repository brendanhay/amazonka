{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.GetVaultNotifications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation retrieves the @notification-configuration@ subresource of
-- the specified vault.
--
-- For information about setting a notification configuration on a vault,
-- see SetVaultNotifications. If a notification configuration for a vault
-- is not set, the operation returns a @404 Not Found@ error. For more
-- information about vault notifications, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier>.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html Get Vault Notification Configuration>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-GetVaultNotifications.html>
module Network.AWS.Glacier.GetVaultNotifications
    (
    -- * Request
      GetVaultNotifications
    -- ** Request constructor
    , getVaultNotifications
    -- ** Request lenses
    , gvnAccountId
    , gvnVaultName

    -- * Response
    , GetVaultNotificationsResponse
    -- ** Response constructor
    , getVaultNotificationsResponse
    -- ** Response lenses
    , gvnrsVaultNotificationConfig
    , gvnrsStatus
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for retrieving the notification configuration set on an
-- Amazon Glacier vault.
--
-- /See:/ 'getVaultNotifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvnAccountId'
--
-- * 'gvnVaultName'
data GetVaultNotifications = GetVaultNotifications'
    { _gvnAccountId :: !Text
    , _gvnVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultNotifications' smart constructor.
getVaultNotifications :: Text -> Text -> GetVaultNotifications
getVaultNotifications pAccountId_ pVaultName_ =
    GetVaultNotifications'
    { _gvnAccountId = pAccountId_
    , _gvnVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
gvnAccountId :: Lens' GetVaultNotifications Text
gvnAccountId = lens _gvnAccountId (\ s a -> s{_gvnAccountId = a});

-- | The name of the vault.
gvnVaultName :: Lens' GetVaultNotifications Text
gvnVaultName = lens _gvnVaultName (\ s a -> s{_gvnVaultName = a});

instance AWSRequest GetVaultNotifications where
        type Sv GetVaultNotifications = Glacier
        type Rs GetVaultNotifications =
             GetVaultNotificationsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 GetVaultNotificationsResponse' <$>
                   (x .?> "vaultNotificationConfig") <*>
                     (pure (fromEnum s)))

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
-- /See:/ 'getVaultNotificationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvnrsVaultNotificationConfig'
--
-- * 'gvnrsStatus'
data GetVaultNotificationsResponse = GetVaultNotificationsResponse'
    { _gvnrsVaultNotificationConfig :: !(Maybe VaultNotificationConfig)
    , _gvnrsStatus                  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetVaultNotificationsResponse' smart constructor.
getVaultNotificationsResponse :: Int -> GetVaultNotificationsResponse
getVaultNotificationsResponse pStatus_ =
    GetVaultNotificationsResponse'
    { _gvnrsVaultNotificationConfig = Nothing
    , _gvnrsStatus = pStatus_
    }

-- | Returns the notification configuration set on the vault.
gvnrsVaultNotificationConfig :: Lens' GetVaultNotificationsResponse (Maybe VaultNotificationConfig)
gvnrsVaultNotificationConfig = lens _gvnrsVaultNotificationConfig (\ s a -> s{_gvnrsVaultNotificationConfig = a});

-- | FIXME: Undocumented member.
gvnrsStatus :: Lens' GetVaultNotificationsResponse Int
gvnrsStatus = lens _gvnrsStatus (\ s a -> s{_gvnrsStatus = a});
