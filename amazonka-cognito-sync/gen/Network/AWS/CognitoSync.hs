-- Module      : Network.AWS.CognitoSync
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Amazon Cognito Sync
--
-- Amazon Cognito Sync provides an AWS service and client library that
-- enable cross-device syncing of application-related user data. High-level
-- client libraries are available for both iOS and Android. You can use
-- these libraries to persist data locally so that it\'s available even if
-- the device is offline. Developer credentials don\'t need to be stored on
-- the mobile device to access the service. You can use Amazon Cognito to
-- obtain a normalized user ID and credentials. User data is persisted in a
-- dataset that can store up to 1 MB of key-value pairs, and you can have
-- up to 20 datasets per user identity.
--
-- With Amazon Cognito Sync, the data stored for each identity is
-- accessible only to credentials assigned to that identity. In order to
-- use the Cognito Sync service, you need to make API calls using
-- credentials retrieved with
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/Welcome.html Amazon Cognito Identity service>.
--
-- If you want to use Cognito Sync in an Android or iOS application, you
-- will probably want to make API calls via the AWS Mobile SDK. To learn
-- more, see the
-- <http://docs.aws.amazon.com/mobile/sdkforandroid/developerguide/cognito-sync.html Developer Guide for Android>
-- and the
-- <http://docs.aws.amazon.com/mobile/sdkforios/developerguide/cognito-sync.html Developer Guide for iOS>.
module Network.AWS.CognitoSync
    ( module Export
    ) where

import           Network.AWS.CognitoSync.BulkPublish                  as Export
import           Network.AWS.CognitoSync.DeleteDataset                as Export
import           Network.AWS.CognitoSync.DescribeDataset              as Export
import           Network.AWS.CognitoSync.DescribeIdentityPoolUsage    as Export
import           Network.AWS.CognitoSync.DescribeIdentityUsage        as Export
import           Network.AWS.CognitoSync.GetBulkPublishDetails        as Export
import           Network.AWS.CognitoSync.GetCognitoEvents             as Export
import           Network.AWS.CognitoSync.GetIdentityPoolConfiguration as Export
import           Network.AWS.CognitoSync.ListDatasets                 as Export
import           Network.AWS.CognitoSync.ListIdentityPoolUsage        as Export
import           Network.AWS.CognitoSync.ListRecords                  as Export
import           Network.AWS.CognitoSync.RegisterDevice               as Export
import           Network.AWS.CognitoSync.SetCognitoEvents             as Export
import           Network.AWS.CognitoSync.SetIdentityPoolConfiguration as Export
import           Network.AWS.CognitoSync.SubscribeToDataset           as Export
import           Network.AWS.CognitoSync.Types                        as Export
import           Network.AWS.CognitoSync.UnsubscribeFromDataset       as Export
import           Network.AWS.CognitoSync.UpdateRecords                as Export
import           Network.AWS.CognitoSync.Waiters                      as Export
