-- Module      : Network.AWS.CognitoSync
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Amazon Cognito Sync provides an AWS service and client library that enable
-- cross-device syncing of application-related user data. High-level client
-- libraries are available for both iOS and Android. You can use these
-- libraries to persist data locally so that it's available even if the device
-- is offline. Developer credentials don't need to be stored on the mobile
-- device to access the service. You can use Amazon Cognito to obtain a
-- normalized user ID and credentials. User data is persisted in a dataset
-- that can store up to 1 MB of key-value pairs, and you can have up to 20
-- datasets per user identity.
module Network.AWS.CognitoSync
    ( module Network.AWS.CognitoSync.DeleteDataset
    , module Network.AWS.CognitoSync.DescribeDataset
    , module Network.AWS.CognitoSync.DescribeIdentityPoolUsage
    , module Network.AWS.CognitoSync.DescribeIdentityUsage
    , module Network.AWS.CognitoSync.GetIdentityPoolConfiguration
    , module Network.AWS.CognitoSync.ListDatasets
    , module Network.AWS.CognitoSync.ListIdentityPoolUsage
    , module Network.AWS.CognitoSync.ListRecords
    , module Network.AWS.CognitoSync.RegisterDevice
    , module Network.AWS.CognitoSync.SetIdentityPoolConfiguration
    , module Network.AWS.CognitoSync.SubscribeToDataset
    , module Network.AWS.CognitoSync.Types
    , module Network.AWS.CognitoSync.UnsubscribeFromDataset
    , module Network.AWS.CognitoSync.UpdateRecords
    ) where

import Network.AWS.CognitoSync.DeleteDataset
import Network.AWS.CognitoSync.DescribeDataset
import Network.AWS.CognitoSync.DescribeIdentityPoolUsage
import Network.AWS.CognitoSync.DescribeIdentityUsage
import Network.AWS.CognitoSync.GetIdentityPoolConfiguration
import Network.AWS.CognitoSync.ListDatasets
import Network.AWS.CognitoSync.ListIdentityPoolUsage
import Network.AWS.CognitoSync.ListRecords
import Network.AWS.CognitoSync.RegisterDevice
import Network.AWS.CognitoSync.SetIdentityPoolConfiguration
import Network.AWS.CognitoSync.SubscribeToDataset
import Network.AWS.CognitoSync.Types
import Network.AWS.CognitoSync.UnsubscribeFromDataset
import Network.AWS.CognitoSync.UpdateRecords
