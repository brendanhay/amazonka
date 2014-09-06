{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the access policies that control access to the
-- domain's document and search endpoints. By default, shows the configuration
-- with any pending changes. Set the Deployed option to true to show the
-- active configuration and exclude pending changes. For more information, see
-- Configuring Access for a Search Domain in the Amazon CloudSearch Developer
-- Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeServiceAccessPolicies
    (
    -- * Request
      DescribeServiceAccessPolicies
    -- ** Request constructor
    , mkDescribeServiceAccessPolicies
    -- ** Request lenses
    , dsapDomainName
    , dsapDeployed

    -- * Response
    , DescribeServiceAccessPoliciesResponse
    -- ** Response lenses
    , dsaprsAccessPolicies
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeServiceAccessPolicies
-- operation. Specifies the name of the domain you want to describe. To show
-- the active configuration and exclude any pending changes, set the Deployed
-- option to true.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies
    { _dsapDomainName :: Text
    , _dsapDeployed :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceAccessPolicies' request.
mkDescribeServiceAccessPolicies :: Text -- ^ 'dsapDomainName'
                                -> DescribeServiceAccessPolicies
mkDescribeServiceAccessPolicies p1 = DescribeServiceAccessPolicies
    { _dsapDomainName = p1
    , _dsapDeployed = Nothing
    }
{-# INLINE mkDescribeServiceAccessPolicies #-}

-- | The name of the domain you want to describe.
dsapDomainName :: Lens' DescribeServiceAccessPolicies Text
dsapDomainName = lens _dsapDomainName (\s a -> s { _dsapDomainName = a })
{-# INLINE dsapDomainName #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dsapDeployed :: Lens' DescribeServiceAccessPolicies (Maybe Bool)
dsapDeployed = lens _dsapDeployed (\s a -> s { _dsapDeployed = a })
{-# INLINE dsapDeployed #-}

instance ToQuery DescribeServiceAccessPolicies where
    toQuery = genericQuery def

-- | The result of a DescribeServiceAccessPolicies request.
newtype DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse
    { _dsaprsAccessPolicies :: AccessPoliciesStatus
    } deriving (Show, Generic)

-- | The access rules configured for the domain specified in the request.
dsaprsAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprsAccessPolicies =
    lens _dsaprsAccessPolicies (\s a -> s { _dsaprsAccessPolicies = a })
{-# INLINE dsaprsAccessPolicies #-}

instance FromXML DescribeServiceAccessPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeServiceAccessPolicies where
    type Sv DescribeServiceAccessPolicies = CloudSearch
    type Rs DescribeServiceAccessPolicies = DescribeServiceAccessPoliciesResponse

    request = post "DescribeServiceAccessPolicies"
    response _ = xmlResponse
