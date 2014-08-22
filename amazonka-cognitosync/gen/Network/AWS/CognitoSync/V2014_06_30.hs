-- Module      : Network.AWS.CognitoSync.V2014_06_30
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
module Network.AWS.CognitoSync.V2014_06_30
    ( module Network.AWS.CognitoSync.V2014_06_30.DeleteDataset
    , module Network.AWS.CognitoSync.V2014_06_30.DescribeDataset
    , module Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityPoolUsage
    , module Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityUsage
    , module Network.AWS.CognitoSync.V2014_06_30.ListDatasets
    , module Network.AWS.CognitoSync.V2014_06_30.ListIdentityPoolUsage
    , module Network.AWS.CognitoSync.V2014_06_30.ListRecords
    , module Network.AWS.CognitoSync.V2014_06_30.Types
    , module Network.AWS.CognitoSync.V2014_06_30.UpdateRecords
    ) where

import Network.AWS.CognitoSync.V2014_06_30.DeleteDataset
import Network.AWS.CognitoSync.V2014_06_30.DescribeDataset
import Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityPoolUsage
import Network.AWS.CognitoSync.V2014_06_30.DescribeIdentityUsage
import Network.AWS.CognitoSync.V2014_06_30.ListDatasets
import Network.AWS.CognitoSync.V2014_06_30.ListIdentityPoolUsage
import Network.AWS.CognitoSync.V2014_06_30.ListRecords
import Network.AWS.CognitoSync.V2014_06_30.Types
import Network.AWS.CognitoSync.V2014_06_30.UpdateRecords
