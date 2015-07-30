{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glacier.SetVaultNotifications
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation configures notifications that will be sent when specific
-- events happen to a vault. By default, you don\'t get any notifications.
--
-- To configure vault notifications, send a PUT request to the
-- @notification-configuration@ subresource of the vault. The request
-- should include a JSON document that provides an Amazon SNS topic and
-- specific events for which you want Amazon Glacier to send notifications
-- to the topic.
--
-- Amazon SNS topics must grant permission to the vault to be allowed to
-- publish notifications to the topic. You can configure a vault to publish
-- a notification for the following vault events:
--
-- -   __ArchiveRetrievalCompleted__ This event occurs when a job that was
--     initiated for an archive retrieval is completed (InitiateJob). The
--     status of the completed job can be \"Succeeded\" or \"Failed\". The
--     notification sent to the SNS topic is the same output as returned
--     from DescribeJob.
-- -   __InventoryRetrievalCompleted__ This event occurs when a job that
--     was initiated for an inventory retrieval is completed (InitiateJob).
--     The status of the completed job can be \"Succeeded\" or \"Failed\".
--     The notification sent to the SNS topic is the same output as
--     returned from DescribeJob.
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
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-put.html Set Vault Notification Configuration>
-- in the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultNotifications.html>
module Network.AWS.Glacier.SetVaultNotifications
    (
    -- * Request
      SetVaultNotifications
    -- ** Request constructor
    , setVaultNotifications
    -- ** Request lenses
    , svnVaultNotificationConfig
    , svnAccountId
    , svnVaultName

    -- * Response
    , SetVaultNotificationsResponse
    -- ** Response constructor
    , setVaultNotificationsResponse
    ) where

import           Network.AWS.Glacier.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Provides options to configure notifications that will be sent when
-- specific events happen to a vault.
--
-- /See:/ 'setVaultNotifications' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'svnVaultNotificationConfig'
--
-- * 'svnAccountId'
--
-- * 'svnVaultName'
data SetVaultNotifications = SetVaultNotifications'
    { _svnVaultNotificationConfig :: !(Maybe VaultNotificationConfig)
    , _svnAccountId               :: !Text
    , _svnVaultName               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetVaultNotifications' smart constructor.
setVaultNotifications :: Text -> Text -> SetVaultNotifications
setVaultNotifications pAccountId_ pVaultName_ =
    SetVaultNotifications'
    { _svnVaultNotificationConfig = Nothing
    , _svnAccountId = pAccountId_
    , _svnVaultName = pVaultName_
    }

-- | Provides options for specifying notification configuration.
svnVaultNotificationConfig :: Lens' SetVaultNotifications (Maybe VaultNotificationConfig)
svnVaultNotificationConfig = lens _svnVaultNotificationConfig (\ s a -> s{_svnVaultNotificationConfig = a});

-- | The @AccountId@ value is the AWS account ID of the account that owns the
-- vault. You can either specify an AWS account ID or optionally a single
-- apos@-@apos (hyphen), in which case Amazon Glacier uses the AWS account
-- ID associated with the credentials used to sign the request. If you use
-- an account ID, do not include any hyphens (apos-apos) in the ID.
svnAccountId :: Lens' SetVaultNotifications Text
svnAccountId = lens _svnAccountId (\ s a -> s{_svnAccountId = a});

-- | The name of the vault.
svnVaultName :: Lens' SetVaultNotifications Text
svnVaultName = lens _svnVaultName (\ s a -> s{_svnVaultName = a});

instance AWSRequest SetVaultNotifications where
        type Sv SetVaultNotifications = Glacier
        type Rs SetVaultNotifications =
             SetVaultNotificationsResponse
        request = putJSON
        response = receiveNull SetVaultNotificationsResponse'

instance ToHeaders SetVaultNotifications where
        toHeaders = const mempty

instance ToJSON SetVaultNotifications where
        toJSON SetVaultNotifications'{..}
          = object
              ["vaultNotificationConfig" .=
                 _svnVaultNotificationConfig]

instance ToPath SetVaultNotifications where
        toPath SetVaultNotifications'{..}
          = mconcat
              ["/", toBS _svnAccountId, "/vaults/",
               toBS _svnVaultName, "/notification-configuration"]

instance ToQuery SetVaultNotifications where
        toQuery = const mempty

-- | /See:/ 'setVaultNotificationsResponse' smart constructor.
data SetVaultNotificationsResponse =
    SetVaultNotificationsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetVaultNotificationsResponse' smart constructor.
setVaultNotificationsResponse :: SetVaultNotificationsResponse
setVaultNotificationsResponse = SetVaultNotificationsResponse'
