{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Configures the access rules that control access to the domain's document
-- and search endpoints. For more information, see Configuring Access for an
-- Amazon CloudSearch Domain.
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_UpdateServiceAccessPolicies.html>
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
    (
    -- * Request
      UpdateServiceAccessPolicies
    -- ** Request constructor
    , updateServiceAccessPolicies
    -- ** Request lenses
    , usapAccessPolicies
    , usapDomainName

    -- * Response
    , UpdateServiceAccessPoliciesResponse
    -- ** Response constructor
    , updateServiceAccessPoliciesResponse
    -- ** Response lenses
    , usaprAccessPolicies
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies
    { _usapAccessPolicies :: Text
    , _usapDomainName     :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'UpdateServiceAccessPolicies' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usapAccessPolicies' @::@ 'Text'
--
-- * 'usapDomainName' @::@ 'Text'
--
updateServiceAccessPolicies :: Text -- ^ 'usapDomainName'
                            -> Text -- ^ 'usapAccessPolicies'
                            -> UpdateServiceAccessPolicies
updateServiceAccessPolicies p1 p2 = UpdateServiceAccessPolicies
    { _usapDomainName     = p1
    , _usapAccessPolicies = p2
    }

-- | The access rules you want to configure. These rules replace any existing
-- rules.
usapAccessPolicies :: Lens' UpdateServiceAccessPolicies Text
usapAccessPolicies =
    lens _usapAccessPolicies (\s a -> s { _usapAccessPolicies = a })

usapDomainName :: Lens' UpdateServiceAccessPolicies Text
usapDomainName = lens _usapDomainName (\s a -> s { _usapDomainName = a })

newtype UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse
    { _usaprAccessPolicies :: AccessPoliciesStatus
    } deriving (Eq, Show, Generic)

-- | 'UpdateServiceAccessPoliciesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'usaprAccessPolicies' @::@ 'AccessPoliciesStatus'
--
updateServiceAccessPoliciesResponse :: AccessPoliciesStatus -- ^ 'usaprAccessPolicies'
                                    -> UpdateServiceAccessPoliciesResponse
updateServiceAccessPoliciesResponse p1 = UpdateServiceAccessPoliciesResponse
    { _usaprAccessPolicies = p1
    }

-- | The access rules configured for the domain.
usaprAccessPolicies :: Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprAccessPolicies =
    lens _usaprAccessPolicies (\s a -> s { _usaprAccessPolicies = a })

instance ToPath UpdateServiceAccessPolicies where
    toPath = const "/"

instance ToQuery UpdateServiceAccessPolicies

instance ToHeaders UpdateServiceAccessPolicies

instance AWSRequest UpdateServiceAccessPolicies where
    type Sv UpdateServiceAccessPolicies = CloudSearch
    type Rs UpdateServiceAccessPolicies = UpdateServiceAccessPoliciesResponse

    request  = post "UpdateServiceAccessPolicies"
    response = xmlResponse

instance FromXML UpdateServiceAccessPoliciesResponse where
    parseXML = withElement "UpdateServiceAccessPoliciesResult" $ \x ->
            <$> x .@ "AccessPolicies"
