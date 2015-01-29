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

-- Module      : Network.AWS.CognitoIdentity.MergeDeveloperIdentities
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

-- | Merges two users having different 'IdentityId's, existing in the same identity
-- pool, and identified by the same developer provider. You can use this action
-- to request that discrete users be merged and identified as a single user in
-- the Cognito environment. Cognito associates the given source user ('SourceUserIdentifier') with the 'IdentityId' of the 'DestinationUserIdentifier'. Only
-- developer-authenticated users can be merged. If the users to be merged are
-- associated with the same public provider, but as two different users, an
-- exception will be thrown.
--
-- <http://docs.aws.amazon.com/cognitoidentity/latest/APIReference/API_MergeDeveloperIdentities.html>
module Network.AWS.CognitoIdentity.MergeDeveloperIdentities
    (
    -- * Request
      MergeDeveloperIdentities
    -- ** Request constructor
    , mergeDeveloperIdentities
    -- ** Request lenses
    , mdiDestinationUserIdentifier
    , mdiDeveloperProviderName
    , mdiIdentityPoolId
    , mdiSourceUserIdentifier

    -- * Response
    , MergeDeveloperIdentitiesResponse
    -- ** Response constructor
    , mergeDeveloperIdentitiesResponse
    -- ** Response lenses
    , mdirIdentityId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.CognitoIdentity.Types
import qualified GHC.Exts

data MergeDeveloperIdentities = MergeDeveloperIdentities
    { _mdiDestinationUserIdentifier :: Text
    , _mdiDeveloperProviderName     :: Text
    , _mdiIdentityPoolId            :: Text
    , _mdiSourceUserIdentifier      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'MergeDeveloperIdentities' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdiDestinationUserIdentifier' @::@ 'Text'
--
-- * 'mdiDeveloperProviderName' @::@ 'Text'
--
-- * 'mdiIdentityPoolId' @::@ 'Text'
--
-- * 'mdiSourceUserIdentifier' @::@ 'Text'
--
mergeDeveloperIdentities :: Text -- ^ 'mdiSourceUserIdentifier'
                         -> Text -- ^ 'mdiDestinationUserIdentifier'
                         -> Text -- ^ 'mdiDeveloperProviderName'
                         -> Text -- ^ 'mdiIdentityPoolId'
                         -> MergeDeveloperIdentities
mergeDeveloperIdentities p1 p2 p3 p4 = MergeDeveloperIdentities
    { _mdiSourceUserIdentifier      = p1
    , _mdiDestinationUserIdentifier = p2
    , _mdiDeveloperProviderName     = p3
    , _mdiIdentityPoolId            = p4
    }

-- | User identifier for the destination user. The value should be a 'DeveloperUserIdentifier'.
mdiDestinationUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiDestinationUserIdentifier =
    lens _mdiDestinationUserIdentifier
        (\s a -> s { _mdiDestinationUserIdentifier = a })

-- | The "domain" by which Cognito will refer to your users. This is a (pseudo)
-- domain name that you provide while creating an identity pool. This name acts
-- as a placeholder that allows your backend and the Cognito service to
-- communicate about the developer provider. For the 'DeveloperProviderName', you
-- can use letters as well as period (.), underscore (_), and dash (-).
mdiDeveloperProviderName :: Lens' MergeDeveloperIdentities Text
mdiDeveloperProviderName =
    lens _mdiDeveloperProviderName
        (\s a -> s { _mdiDeveloperProviderName = a })

-- | An identity pool ID in the format REGION:GUID.
mdiIdentityPoolId :: Lens' MergeDeveloperIdentities Text
mdiIdentityPoolId =
    lens _mdiIdentityPoolId (\s a -> s { _mdiIdentityPoolId = a })

-- | User identifier for the source user. The value should be a 'DeveloperUserIdentifier'.
mdiSourceUserIdentifier :: Lens' MergeDeveloperIdentities Text
mdiSourceUserIdentifier =
    lens _mdiSourceUserIdentifier (\s a -> s { _mdiSourceUserIdentifier = a })

newtype MergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse
    { _mdirIdentityId :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

-- | 'MergeDeveloperIdentitiesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mdirIdentityId' @::@ 'Maybe' 'Text'
--
mergeDeveloperIdentitiesResponse :: MergeDeveloperIdentitiesResponse
mergeDeveloperIdentitiesResponse = MergeDeveloperIdentitiesResponse
    { _mdirIdentityId = Nothing
    }

-- | A unique identifier in the format REGION:GUID.
mdirIdentityId :: Lens' MergeDeveloperIdentitiesResponse (Maybe Text)
mdirIdentityId = lens _mdirIdentityId (\s a -> s { _mdirIdentityId = a })

instance ToPath MergeDeveloperIdentities where
    toPath = const "/"

instance ToQuery MergeDeveloperIdentities where
    toQuery = const mempty

instance ToHeaders MergeDeveloperIdentities

instance ToJSON MergeDeveloperIdentities where
    toJSON MergeDeveloperIdentities{..} = object
        [ "SourceUserIdentifier"      .= _mdiSourceUserIdentifier
        , "DestinationUserIdentifier" .= _mdiDestinationUserIdentifier
        , "DeveloperProviderName"     .= _mdiDeveloperProviderName
        , "IdentityPoolId"            .= _mdiIdentityPoolId
        ]

instance AWSRequest MergeDeveloperIdentities where
    type Sv MergeDeveloperIdentities = CognitoIdentity
    type Rs MergeDeveloperIdentities = MergeDeveloperIdentitiesResponse

    request  = post "MergeDeveloperIdentities"
    response = jsonResponse

instance FromJSON MergeDeveloperIdentitiesResponse where
    parseJSON = withObject "MergeDeveloperIdentitiesResponse" $ \o -> MergeDeveloperIdentitiesResponse
        <$> o .:? "IdentityId"
