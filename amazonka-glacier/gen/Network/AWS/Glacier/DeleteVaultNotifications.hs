{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.DeleteVaultNotifications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the notification configuration set for a vault.
-- The operation is eventually consistent; that is, it might take some time
-- for Amazon Glacier to completely disable the notifications and you might
-- still receive some notifications for a short time after you send the
-- delete request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don\'t have any
-- permissions by default. You must grant them explicit permission to
-- perform specific actions. For more information, see
-- <http://docs.aws.amazon.com/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier>
-- and
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html Delete Vault Notification Configuration>
-- in the Amazon Glacier Developer Guide.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-DeleteVaultNotifications.html>
module Network.AWS.Glacier.DeleteVaultNotifications
    (
    -- * Request
      DeleteVaultNotifications
    -- ** Request constructor
    , deleteVaultNotifications
    -- ** Request lenses
    , dvnAccountId
    , dvnVaultName

    -- * Response
    , DeleteVaultNotificationsResponse
    -- ** Response constructor
    , deleteVaultNotificationsResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options for deleting a vault notification configuration from an
-- Amazon Glacier vault.
--
-- /See:/ 'deleteVaultNotifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvnAccountId'
--
-- * 'dvnVaultName'
data DeleteVaultNotifications = DeleteVaultNotifications'
    { _dvnAccountId :: !Text
    , _dvnVaultName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVaultNotifications' smart constructor.
deleteVaultNotifications :: Text -> Text -> DeleteVaultNotifications
deleteVaultNotifications pAccountId_ pVaultName_ =
    DeleteVaultNotifications'
    { _dvnAccountId = pAccountId_
    , _dvnVaultName = pVaultName_
    }

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
dvnAccountId :: Lens' DeleteVaultNotifications Text
dvnAccountId = lens _dvnAccountId (\ s a -> s{_dvnAccountId = a});

-- | The name of the vault.
dvnVaultName :: Lens' DeleteVaultNotifications Text
dvnVaultName = lens _dvnVaultName (\ s a -> s{_dvnVaultName = a});

instance AWSRequest DeleteVaultNotifications where
        type Sv DeleteVaultNotifications = Glacier
        type Rs DeleteVaultNotifications =
             DeleteVaultNotificationsResponse
        request = delete
        response
          = receiveNull DeleteVaultNotificationsResponse'

instance ToHeaders DeleteVaultNotifications where
        toHeaders = const mempty

instance ToPath DeleteVaultNotifications where
        toPath DeleteVaultNotifications'{..}
          = mconcat
              ["/", toPath _dvnAccountId, "/vaults/",
               toPath _dvnVaultName, "/notification-configuration"]

instance ToQuery DeleteVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'deleteVaultNotificationsResponse' smart constructor.
data DeleteVaultNotificationsResponse =
    DeleteVaultNotificationsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVaultNotificationsResponse' smart constructor.
deleteVaultNotificationsResponse :: DeleteVaultNotificationsResponse
deleteVaultNotificationsResponse = DeleteVaultNotificationsResponse'
