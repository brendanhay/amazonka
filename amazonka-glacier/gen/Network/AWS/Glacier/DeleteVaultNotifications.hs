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

-- Module      : Network.AWS.Glacier.DeleteVaultNotifications
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

-- | This operation deletes the notification configuration set for a vault. The
-- operation is eventually consistent;that is, it might take some time for
-- Amazon Glacier to completely disable the notifications and you might still
-- receive some notifications for a short time after you send the delete
-- request.
--
-- An AWS account has full permission to perform all operations (actions).
-- However, AWS Identity and Access Management (IAM) users don't have any
-- permissions by default. You must grant them explicit permission to perform
-- specific actions. For more information, see <http://docs.aws.amazon.com/latest/dev/using-iam-with-amazon-glacier.html Access Control Using AWS Identityand Access Management (IAM)>.
--
-- For conceptual information and underlying REST API, go to <http://docs.aws.amazon.com/amazonglacier/latest/dev/configuring-notifications.html Configuring VaultNotifications in Amazon Glacier> and <http://docs.aws.amazon.com/amazonglacier/latest/dev/api-vault-notifications-delete.html Delete Vault Notification Configuration >
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

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.Glacier.Types
import qualified GHC.Exts

data DeleteVaultNotifications = DeleteVaultNotifications
    { _dvnAccountId :: Text
    , _dvnVaultName :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVaultNotifications' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvnAccountId' @::@ 'Text'
--
-- * 'dvnVaultName' @::@ 'Text'
--
deleteVaultNotifications :: Text -- ^ 'dvnAccountId'
                         -> Text -- ^ 'dvnVaultName'
                         -> DeleteVaultNotifications
deleteVaultNotifications p1 p2 = DeleteVaultNotifications
    { _dvnAccountId = p1
    , _dvnVaultName = p2
    }

-- | The 'AccountId' is the AWS Account ID. You can specify either the AWS Account
-- ID or optionally a '-', in which case Amazon Glacier uses the AWS Account ID
-- associated with the credentials used to sign the request. If you specify your
-- Account ID, do not include hyphens in it.
dvnAccountId :: Lens' DeleteVaultNotifications Text
dvnAccountId = lens _dvnAccountId (\s a -> s { _dvnAccountId = a })

-- | The name of the vault.
dvnVaultName :: Lens' DeleteVaultNotifications Text
dvnVaultName = lens _dvnVaultName (\s a -> s { _dvnVaultName = a })

data DeleteVaultNotificationsResponse = DeleteVaultNotificationsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DeleteVaultNotificationsResponse' constructor.
deleteVaultNotificationsResponse :: DeleteVaultNotificationsResponse
deleteVaultNotificationsResponse = DeleteVaultNotificationsResponse

instance ToPath DeleteVaultNotifications where
    toPath DeleteVaultNotifications{..} = mconcat
        [ "/"
        , toText _dvnAccountId
        , "/vaults/"
        , toText _dvnVaultName
        , "/notification-configuration"
        ]

instance ToQuery DeleteVaultNotifications where
    toQuery = const mempty

instance ToHeaders DeleteVaultNotifications

instance ToJSON DeleteVaultNotifications where
    toJSON = const (toJSON Empty)

instance AWSRequest DeleteVaultNotifications where
    type Sv DeleteVaultNotifications = Glacier
    type Rs DeleteVaultNotifications = DeleteVaultNotificationsResponse

    request  = delete
    response = nullResponse DeleteVaultNotificationsResponse
