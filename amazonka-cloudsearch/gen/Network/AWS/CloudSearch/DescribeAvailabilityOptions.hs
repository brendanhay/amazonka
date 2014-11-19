{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeAvailabilityOptions.html>
module Network.AWS.CloudSearch.DescribeAvailabilityOptions
    (
    -- * Request
      DescribeAvailabilityOptions
    -- ** Request constructor
    , describeAvailabilityOptions
    -- ** Request lenses
    , daoDeployed
    , daoDomainName

    -- * Response
    , DescribeAvailabilityOptionsResponse
    -- ** Response constructor
    , describeAvailabilityOptionsResponse
    -- ** Response lenses
    , daorAvailabilityOptions
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DescribeAvailabilityOptions = DescribeAvailabilityOptions
    { _daoDeployed   :: Maybe Bool
    , _daoDomainName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeAvailabilityOptions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daoDeployed' @::@ 'Maybe' 'Bool'
--
-- * 'daoDomainName' @::@ 'Text'
--
describeAvailabilityOptions :: Text -- ^ 'daoDomainName'
                            -> DescribeAvailabilityOptions
describeAvailabilityOptions p1 = DescribeAvailabilityOptions
    { _daoDomainName = p1
    , _daoDeployed   = Nothing
    }

-- | Whether to display the deployed configuration (true) or include any
-- pending changes (false). Defaults to false.
daoDeployed :: Lens' DescribeAvailabilityOptions (Maybe Bool)
daoDeployed = lens _daoDeployed (\s a -> s { _daoDeployed = a })

-- | The name of the domain you want to describe.
daoDomainName :: Lens' DescribeAvailabilityOptions Text
daoDomainName = lens _daoDomainName (\s a -> s { _daoDomainName = a })

newtype DescribeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse
    { _daorAvailabilityOptions :: Maybe AvailabilityOptionsStatus
    } deriving (Eq, Show, Generic)

-- | 'DescribeAvailabilityOptionsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daorAvailabilityOptions' @::@ 'Maybe' 'AvailabilityOptionsStatus'
--
describeAvailabilityOptionsResponse :: DescribeAvailabilityOptionsResponse
describeAvailabilityOptionsResponse = DescribeAvailabilityOptionsResponse
    { _daorAvailabilityOptions = Nothing
    }

-- | The availability options configured for the domain. Indicates whether
-- Multi-AZ is enabled for the domain.
daorAvailabilityOptions :: Lens' DescribeAvailabilityOptionsResponse (Maybe AvailabilityOptionsStatus)
daorAvailabilityOptions =
    lens _daorAvailabilityOptions (\s a -> s { _daorAvailabilityOptions = a })

instance ToPath DescribeAvailabilityOptions where
    toPath = const "/"

instance ToQuery DescribeAvailabilityOptions

instance ToHeaders DescribeAvailabilityOptions

instance AWSRequest DescribeAvailabilityOptions where
    type Sv DescribeAvailabilityOptions = CloudSearch
    type Rs DescribeAvailabilityOptions = DescribeAvailabilityOptionsResponse

    request  = post "DescribeAvailabilityOptions"
    response = xmlResponse

instance FromXML DescribeAvailabilityOptionsResponse where
    parseXML = withElement "DescribeAvailabilityOptionsResult" $ \x ->
        DescribeAvailabilityOptionsResponse
            <$> x .@? "AvailabilityOptions"
