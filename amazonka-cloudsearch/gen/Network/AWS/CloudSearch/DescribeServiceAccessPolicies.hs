{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch
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
module Network.AWS.CloudSearch
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
    -- ** Response constructor
    , mkDescribeServiceAccessPoliciesResponse
    -- ** Response lenses
    , dsaprAccessPolicies
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeServiceAccessPolicies
-- operation. Specifies the name of the domain you want to describe. To show
-- the active configuration and exclude any pending changes, set the Deployed
-- option to true.
data DescribeServiceAccessPolicies = DescribeServiceAccessPolicies
    { _dsapDomainName :: !Text
    , _dsapDeployed :: !(Maybe Bool)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceAccessPolicies' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Deployed ::@ @Maybe Bool@
--
mkDescribeServiceAccessPolicies :: Text -- ^ 'dsapDomainName'
                                -> DescribeServiceAccessPolicies
mkDescribeServiceAccessPolicies p1 = DescribeServiceAccessPolicies
    { _dsapDomainName = p1
    , _dsapDeployed = Nothing
    }

-- | The name of the domain you want to describe.
dsapDomainName :: Lens' DescribeServiceAccessPolicies Text
dsapDomainName = lens _dsapDomainName (\s a -> s { _dsapDomainName = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dsapDeployed :: Lens' DescribeServiceAccessPolicies (Maybe Bool)
dsapDeployed = lens _dsapDeployed (\s a -> s { _dsapDeployed = a })

instance ToQuery DescribeServiceAccessPolicies where
    toQuery = genericQuery def

-- | The result of a DescribeServiceAccessPolicies request.
newtype DescribeServiceAccessPoliciesResponse = DescribeServiceAccessPoliciesResponse
    { _dsaprAccessPolicies :: AccessPoliciesStatus
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeServiceAccessPoliciesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AccessPolicies ::@ @AccessPoliciesStatus@
--
mkDescribeServiceAccessPoliciesResponse :: AccessPoliciesStatus -- ^ 'dsaprAccessPolicies'
                                        -> DescribeServiceAccessPoliciesResponse
mkDescribeServiceAccessPoliciesResponse p1 = DescribeServiceAccessPoliciesResponse
    { _dsaprAccessPolicies = p1
    }

-- | The access rules configured for the domain specified in the request.
dsaprAccessPolicies :: Lens' DescribeServiceAccessPoliciesResponse AccessPoliciesStatus
dsaprAccessPolicies =
    lens _dsaprAccessPolicies (\s a -> s { _dsaprAccessPolicies = a })

instance FromXML DescribeServiceAccessPoliciesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeServiceAccessPolicies where
    type Sv DescribeServiceAccessPolicies = CloudSearch
    type Rs DescribeServiceAccessPolicies = DescribeServiceAccessPoliciesResponse

    request = post "DescribeServiceAccessPolicies"
    response _ = xmlResponse
