{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.UpdateServiceAccessPolicies
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
module Network.AWS.CloudSearch.V2013_01_01.UpdateServiceAccessPolicies
    (
    -- * Request
      UpdateServiceAccessPolicies
    -- ** Request constructor
    , mkUpdateServiceAccessPolicies
    -- ** Request lenses
    , usapDomainName
    , usapAccessPolicies

    -- * Response
    , UpdateServiceAccessPoliciesResponse
    -- ** Response lenses
    , usaprsAccessPolicies
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the UpdateServiceAccessPolicies operation.
-- Specifies the name of the domain you want to update and the access rules
-- you want to configure.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies
    { _usapDomainName :: Text
    , _usapAccessPolicies :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'UpdateServiceAccessPolicies' request.
mkUpdateServiceAccessPolicies :: Text -- ^ 'usapDomainName'
                              -> Text -- ^ 'usapAccessPolicies'
                              -> UpdateServiceAccessPolicies
mkUpdateServiceAccessPolicies p1 p2 = UpdateServiceAccessPolicies
    { _usapDomainName = p1
    , _usapAccessPolicies = p2
    }

-- | A string that represents the name of a domain. Domain names are unique
-- across the domains owned by an account within an AWS region. Domain names
-- start with a letter or number and can contain the following characters: a-z
-- (lowercase), 0-9, and - (hyphen).
usapDomainName :: Lens' UpdateServiceAccessPolicies Text
usapDomainName = lens _usapDomainName (\s a -> s { _usapDomainName = a })

-- | The access rules you want to configure. These rules replace any existing
-- rules.
usapAccessPolicies :: Lens' UpdateServiceAccessPolicies Text
usapAccessPolicies =
    lens _usapAccessPolicies (\s a -> s { _usapAccessPolicies = a })

instance ToQuery UpdateServiceAccessPolicies where
    toQuery = genericQuery def

-- | The result of an UpdateServiceAccessPolicies request. Contains the new
-- access policies.
newtype UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse
    { _usaprsAccessPolicies :: AccessPoliciesStatus
    } deriving (Show, Generic)

-- | The access rules configured for the domain.
usaprsAccessPolicies :: Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
usaprsAccessPolicies =
    lens _usaprsAccessPolicies (\s a -> s { _usaprsAccessPolicies = a })

instance FromXML UpdateServiceAccessPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest UpdateServiceAccessPolicies where
    type Sv UpdateServiceAccessPolicies = CloudSearch
    type Rs UpdateServiceAccessPolicies = UpdateServiceAccessPoliciesResponse

    request = post "UpdateServiceAccessPolicies"
    response _ = xmlResponse
