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

-- Module      : Network.AWS.Glacier.GetVaultNotifications
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

-- | This operation retrieves the 'notification-configuration' subresource of the
-- specified vault.
--
-- For information about setting a notification configuration on a vault, see 'SetVaultNotifications'. If a notification configuration for a vault is not set, the operation
-- returns a '404 Not Found' error. For more information about vault
-- notifications, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring Vault Notifications in Amazon Glacier>.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/amazonglacier/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring VaultNotifications in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-get.html Get Vault Notification Configuration > in
-- the /Amazon Glacier Developer Guide/.
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
    , gvnrVaultNotificationConfig
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data GetVaultNotifications = GetVaultNotifications
    { _gvnAccountId :: Text
    , _gvnVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'GetVaultNotifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvnAccountId' @::@ 'Text'
--
-- * 'gvnVaultName' @::@ 'Text'
--
getVaultNotifications :: Text -- ^ 'gvnAccountId'
                      -> Text -- ^ 'gvnVaultName'
                      -> GetVaultNotifications
getVaultNotifications p1 p2 = GetVaultNotifications
    { _gvnAccountId = p1
    , _gvnVaultName = p2
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
gvnAccountId :: Lens' GetVaultNotifications Text
gvnAccountId = lens _gvnAccountId (\s a -> s { _gvnAccountId = a })

-- | The name of the vault.
gvnVaultName :: Lens' GetVaultNotifications Text
gvnVaultName = lens _gvnVaultName (\s a -> s { _gvnVaultName = a })

newtype GetVaultNotificationsResponse = GetVaultNotificationsResponse
    { _gvnrVaultNotificationConfig :: Maybe VaultNotificationConfig
    } deriving (Eq, Read, Show)

-- | 'GetVaultNotificationsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gvnrVaultNotificationConfig' @::@ 'Maybe' 'VaultNotificationConfig'
--
getVaultNotificationsResponse :: GetVaultNotificationsResponse
getVaultNotificationsResponse = GetVaultNotificationsResponse
    { _gvnrVaultNotificationConfig = Nothing
    }

-- | Returns the notification configuration set on the vault.
gvnrVaultNotificationConfig :: Lens' GetVaultNotificationsResponse (Maybe VaultNotificationConfig)
gvnrVaultNotificationConfig =
    lens _gvnrVaultNotificationConfig
        (\s a -> s { _gvnrVaultNotificationConfig = a })

instance ToPath GetVaultNotifications where
    toPath GetVaultNotifications{..} = mconcat
        [ "/"
        , toText _gvnAccountId
        , "/vaults/"
        , toText _gvnVaultName
        , "/notification-configuration"
        ]

instance ToQuery GetVaultNotifications where
    toQuery = const mempty

instance ToHeaders GetVaultNotifications

instance ToJSON GetVaultNotifications where
    toJSON = const (toJSON Empty)

instance AWSRequest GetVaultNotifications where
    type Sv GetVaultNotifications = Glacier
    type Rs GetVaultNotifications = GetVaultNotificationsResponse

    request  = get
    response = jsonResponse

instance FromJSON GetVaultNotificationsResponse where
    parseJSON = withObject "GetVaultNotificationsResponse" $ \o -> GetVaultNotificationsResponse
        <$> o .:? "vaultNotificationConfig"
