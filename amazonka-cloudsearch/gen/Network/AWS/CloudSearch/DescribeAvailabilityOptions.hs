{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeAvailabilityOptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets the availability options configured for a domain. By default, shows
-- the configuration with any pending changes. Set the Deployed option to true
-- to show the active configuration and exclude pending changes. For more
-- information, see Configuring Availability Options in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
    (
    -- * Request
      DescribeAvailabilityOptions
    -- ** Request constructor
    , describeAvailabilityOptions
    -- ** Request lenses
    , dao2DomainName
    , dao2Deployed

    -- * Response
    , DescribeAvailabilityOptionsResponse
    -- ** Response constructor
    , describeAvailabilityOptionsResponse
    -- ** Response lenses
    , daorrAvailabilityOptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeAvailabilityOptions operation.
-- Specifies the name of the domain you want to describe. To show the active
-- configuration and exclude any pending changes, set the Deployed option to
-- true.
data DescribeAvailabilityOptions = DescribeAvailabilityOptions
    { _dao2DomainName :: Text
    , _dao2Deployed :: Maybe Bool
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAvailabilityOptions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Deployed ::@ @Maybe Bool@
--
describeAvailabilityOptions :: Text -- ^ 'dao2DomainName'
                            -> DescribeAvailabilityOptions
describeAvailabilityOptions p1 = DescribeAvailabilityOptions
    { _dao2DomainName = p1
    , _dao2Deployed = Nothing
    }

-- | The name of the domain you want to describe.
dao2DomainName :: Lens' DescribeAvailabilityOptions Text
dao2DomainName = lens _dao2DomainName (\s a -> s { _dao2DomainName = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dao2Deployed :: Lens' DescribeAvailabilityOptions (Maybe Bool)
dao2Deployed = lens _dao2Deployed (\s a -> s { _dao2Deployed = a })

instance ToQuery DescribeAvailabilityOptions where
    toQuery = genericQuery def

-- | The result of a DescribeAvailabilityOptions request. Indicates whether or
-- not the Multi-AZ option is enabled for the domain specified in the request.
newtype DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse
    { _daorrAvailabilityOptions :: Maybe AvailabilityOptionsStatus
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeAvailabilityOptionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AvailabilityOptions ::@ @Maybe AvailabilityOptionsStatus@
--
describeAvailabilityOptionsResponse :: DescribeAvailabilityOptionsResponse
describeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse
    { _daorrAvailabilityOptions = Nothing
    }

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
daorrAvailabilityOptions :: Lens' DescribeAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
daorrAvailabilityOptions =
    lens _daorrAvailabilityOptions
         (\s a -> s { _daorrAvailabilityOptions = a })

instance FromXML DescribeAvailabilityOptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeAvailabilityOptions where
    type Sv DescribeAvailabilityOptions = CloudSearch
    type Rs DescribeAvailabilityOptions = DescribeAvailabilityOptionsResponse

    request = post "DescribeAvailabilityOptions"
    response _ = xmlResponse
