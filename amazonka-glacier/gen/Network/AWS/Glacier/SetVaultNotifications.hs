{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Glacier.SetVaultNotifications
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation configures notifications that will be sent when specific
-- events happen to a vault. By default, you don't get any notifications.
--
-- To configure vault notifications, send a PUT request to the 'notification-configuration' subresource of the vault. The request should include a JSON document that
-- provides an Amazon SNS topic and specific events for which you want Amazon
-- Glacier to send notifications to the topic.
--
-- Amazon SNS topics must grant permission to the vault to be allowed to
-- publish notifications to the topic. You can configure a vault to publish a
-- notification for the following vault events:
--
-- ArchiveRetrievalCompleted This event occurs when a job that was initiated
-- for an archive retrieval is completed ('InitiateJob'). The status of the
-- completed job can be "Succeeded" or "Failed". The notification sent to the
-- SNS topic is the same output as returned from 'DescribeJob'.   InventoryRetrievalCompleted
-- This event occurs when a job that was initiated for an inventory retrieval
-- is completed ('InitiateJob'). The status of the completed job can be
-- "Succeeded" or "Failed". The notification sent to the SNS topic is the same
-- output as returned from 'DescribeJob'.   An AWS account has full permission to
-- perform all operations (actions). However, AWS Identity and Access Management
-- (IAM) users don't have any permissions by default. You must grant them
-- explicit permission to perform specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identity and Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring VaultNotifications in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-put.html Set Vault Notification Configuration > in
-- the /Amazon Glacier Developer Guide/.
--
-- <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-SetVaultNotifications.html>
module Network.AWS.Glacier.SetVaultNotifications
    (
    -- * Request
      SetVaultNotifications
    -- ** Request constructor
    , setVaultNotifications
    -- ** Request lenses
    , svnAccountId
    , svnVaultName
    , svnVaultNotificationConfig

    -- * Response
    , SetVaultNotificationsResponse
    -- ** Response constructor
    , setVaultNotificationsResponse
    ) where

import Network.AWS.Data (Object)
import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data SetVaultNotifications = SetVaultNotifications
    { _svnAccountId               :: Text
    , _svnVaultName               :: Text
    , _svnVaultNotificationConfig :: Maybe VaultNotificationConfig
    } deriving (Eq, Read, Show)

-- | 'SetVaultNotifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'svnAccountId' @::@ 'Text'
--
-- * 'svnVaultName' @::@ 'Text'
--
-- * 'svnVaultNotificationConfig' @::@ 'Maybe' 'VaultNotificationConfig'
--
setVaultNotifications :: Text -- ^ 'svnAccountId'
                      -> Text -- ^ 'svnVaultName'
                      -> SetVaultNotifications
setVaultNotifications p1 p2 = SetVaultNotifications
    { _svnAccountId               = p1
    , _svnVaultName               = p2
    , _svnVaultNotificationConfig = Nothing
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
svnAccountId :: Lens' SetVaultNotifications Text
svnAccountId = lens _svnAccountId (\s a -> s { _svnAccountId = a })

-- | The name of the vault.
svnVaultName :: Lens' SetVaultNotifications Text
svnVaultName = lens _svnVaultName (\s a -> s { _svnVaultName = a })

-- | Provides options for specifying notification configuration.
svnVaultNotificationConfig :: Lens' SetVaultNotifications (Maybe VaultNotificationConfig)
svnVaultNotificationConfig =
    lens _svnVaultNotificationConfig
        (\s a -> s { _svnVaultNotificationConfig = a })

data SetVaultNotificationsResponse = SetVaultNotificationsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'SetVaultNotificationsResponse' constructor.
setVaultNotificationsResponse :: SetVaultNotificationsResponse
setVaultNotificationsResponse = SetVaultNotificationsResponse

instance ToPath SetVaultNotifications where
    toPath SetVaultNotifications{..} = mconcat
        [ "/"
        , toText _svnAccountId
        , "/vaults/"
        , toText _svnVaultName
        , "/notification-configuration"
        ]

instance ToQuery SetVaultNotifications where
    toQuery = const mempty

instance ToHeaders SetVaultNotifications

instance ToJSON SetVaultNotifications where
    toJSON SetVaultNotifications{..} = object
        [ "vaultNotificationConfig" .= _svnVaultNotificationConfig
        ]

instance AWSRequest SetVaultNotifications where
    type Sv SetVaultNotifications = Glacier
    type Rs SetVaultNotifications = SetVaultNotificationsResponse

    request  = put
    response = nullResponse SetVaultNotificationsResponse
