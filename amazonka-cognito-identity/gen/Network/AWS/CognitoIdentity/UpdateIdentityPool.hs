{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoIdentity.UpdateIdentityPool
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Updates a user pool. UpdateIdentityPool The following are a request and
-- response for the UpdateIdentityPool action. { "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "IdentityPoolDescription": "An identity pool that needs
-- updating", "Unauthenticated": true, "SupportedLoginProviders": {
-- "www.amazon.com": "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" } } { "IdentityPoolDescription": "An
-- identity pool that needs updating", "IdentityPoolId":
-- "us-east-1:1a234b56-7890-1cd2-3e45-f6g7hEXAMPLE", "IdentityPoolName":
-- "MyUpdatedPool", "SupportedLoginProviders": { "www.amazon.com":
-- "Amazon_App_ID", "graph.facebook.com": "Facebook_App_ID",
-- "accounts.google.com": "Google_App_ID" }, "AllowUnauthenticatedIdentities":
-- true }.
module Network.AWS.CognitoIdentity.UpdateIdentityPool
    (
    -- * Request
      UpdateIdentityPool
    -- ** Request constructor
    , updateIdentityPool
    -- ** Request lenses
    , uipIdentityPoolId
    , uipIdentityPoolName
    , uipAllowUnauthenticatedIdentities
    , uipSupportedLoginProviders

    -- * Response
    , UpdateIdentityPoolResponse
    -- ** Response constructor
    , updateIdentityPoolResponse
    -- ** Response lenses
    , uiprIdentityPoolId
    , uiprIdentityPoolName
    , uiprAllowUnauthenticatedIdentities
    , uiprSupportedLoginProviders
    ) where

import Network.AWS.CognitoIdentity.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | An object representing a Cognito identity pool.
data UpdateIdentityPool = UpdateIdentityPool
    { _uipIdentityPoolId :: Text
    , _uipIdentityPoolName :: Text
    , _uipAllowUnauthenticatedIdentities :: !Bool
    , _uipSupportedLoginProviders :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateIdentityPool' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
-- * @IdentityPoolName ::@ @Text@
--
-- * @AllowUnauthenticatedIdentities ::@ @Bool@
--
-- * @SupportedLoginProviders ::@ @Map Text Text@
--
updateIdentityPool :: Text -- ^ 'uipIdentityPoolId'
                   -> Text -- ^ 'uipIdentityPoolName'
                   -> Bool -- ^ 'uipAllowUnauthenticatedIdentities'
                   -> UpdateIdentityPool
updateIdentityPool p1 p2 p3 = UpdateIdentityPool
    { _uipIdentityPoolId = p1
    , _uipIdentityPoolName = p2
    , _uipAllowUnauthenticatedIdentities = p3
    , _uipSupportedLoginProviders = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
uipIdentityPoolId :: Lens' UpdateIdentityPool Text
uipIdentityPoolId =
    lens _uipIdentityPoolId (\s a -> s { _uipIdentityPoolId = a })

-- | A string that you provide.
uipIdentityPoolName :: Lens' UpdateIdentityPool Text
uipIdentityPoolName =
    lens _uipIdentityPoolName (\s a -> s { _uipIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
uipAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPool Bool
uipAllowUnauthenticatedIdentities =
    lens _uipAllowUnauthenticatedIdentities
         (\s a -> s { _uipAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
uipSupportedLoginProviders :: Lens' UpdateIdentityPool (Map Text Text)
uipSupportedLoginProviders =
    lens _uipSupportedLoginProviders
         (\s a -> s { _uipSupportedLoginProviders = a })

instance ToPath UpdateIdentityPool

instance ToQuery UpdateIdentityPool

instance ToHeaders UpdateIdentityPool

instance ToJSON UpdateIdentityPool

-- | An object representing a Cognito identity pool.
data UpdateIdentityPoolResponse = UpdateIdentityPoolResponse
    { _uiprIdentityPoolId :: Text
    , _uiprIdentityPoolName :: Text
    , _uiprAllowUnauthenticatedIdentities :: !Bool
    , _uiprSupportedLoginProviders :: Map Text Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateIdentityPoolResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @IdentityPoolId ::@ @Text@
--
-- * @IdentityPoolName ::@ @Text@
--
-- * @AllowUnauthenticatedIdentities ::@ @Bool@
--
-- * @SupportedLoginProviders ::@ @Map Text Text@
--
updateIdentityPoolResponse :: Text -- ^ 'uiprIdentityPoolId'
                           -> Text -- ^ 'uiprIdentityPoolName'
                           -> Bool -- ^ 'uiprAllowUnauthenticatedIdentities'
                           -> UpdateIdentityPoolResponse
updateIdentityPoolResponse p1 p2 p3 = UpdateIdentityPoolResponse
    { _uiprIdentityPoolId = p1
    , _uiprIdentityPoolName = p2
    , _uiprAllowUnauthenticatedIdentities = p3
    , _uiprSupportedLoginProviders = mempty
    }

-- | An identity pool ID in the format REGION:GUID.
uiprIdentityPoolId :: Lens' UpdateIdentityPoolResponse Text
uiprIdentityPoolId =
    lens _uiprIdentityPoolId (\s a -> s { _uiprIdentityPoolId = a })

-- | A string that you provide.
uiprIdentityPoolName :: Lens' UpdateIdentityPoolResponse Text
uiprIdentityPoolName =
    lens _uiprIdentityPoolName (\s a -> s { _uiprIdentityPoolName = a })

-- | TRUE if the identity pool supports unauthenticated logins.
uiprAllowUnauthenticatedIdentities :: Lens' UpdateIdentityPoolResponse Bool
uiprAllowUnauthenticatedIdentities =
    lens _uiprAllowUnauthenticatedIdentities
         (\s a -> s { _uiprAllowUnauthenticatedIdentities = a })

-- | Optional key:value pairs mapping provider names to provider app IDs.
uiprSupportedLoginProviders :: Lens' UpdateIdentityPoolResponse (Map Text Text)
uiprSupportedLoginProviders =
    lens _uiprSupportedLoginProviders
         (\s a -> s { _uiprSupportedLoginProviders = a })

instance FromJSON UpdateIdentityPoolResponse

instance AWSRequest UpdateIdentityPool where
    type Sv UpdateIdentityPool = CognitoIdentity
    type Rs UpdateIdentityPool = UpdateIdentityPoolResponse

    request = get
    response _ = jsonResponse
