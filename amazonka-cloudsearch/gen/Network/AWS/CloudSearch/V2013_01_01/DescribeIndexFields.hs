{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.V2013_01_01.DescribeIndexFields
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Gets information about the index fields configured for the search domain.
-- Can be limited to specific fields by name. By default, shows all fields and
-- includes any pending changes to the configuration. Set the Deployed option
-- to true to show the active configuration and exclude pending changes. For
-- more information, see Getting Domain Information in the Amazon CloudSearch
-- Developer Guide.
module Network.AWS.CloudSearch.V2013_01_01.DescribeIndexFields
    (
    -- * Request
      DescribeIndexFields
    -- ** Request constructor
    , describeIndexFields
    -- ** Request lenses
    , difvDomainName
    , difvDeployed
    , difvFieldNames

    -- * Response
    , DescribeIndexFieldsResponse
    -- ** Response lenses
    , difwIndexFields
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeIndexFields' request.
describeIndexFields :: Text -- ^ 'difvDomainName'
                    -> DescribeIndexFields
describeIndexFields p1 = DescribeIndexFields
    { _difvDomainName = p1
    , _difvDeployed = Nothing
    , _difvFieldNames = mempty
    }

data DescribeIndexFields = DescribeIndexFields
    { _difvDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _difvDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _difvFieldNames :: [Text]
      -- ^ A list of the index fields you want to describe. If not
      -- specified, information is returned for all configured index
      -- fields.
    } deriving (Show, Generic)

-- | The name of the domain you want to describe.
difvDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeIndexFields
    -> f DescribeIndexFields
difvDomainName f x =
    (\y -> x { _difvDomainName = y })
       <$> f (_difvDomainName x)
{-# INLINE difvDomainName #-}

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
difvDeployed
    :: Functor f
    => (Maybe Bool
    -> f (Maybe Bool))
    -> DescribeIndexFields
    -> f DescribeIndexFields
difvDeployed f x =
    (\y -> x { _difvDeployed = y })
       <$> f (_difvDeployed x)
{-# INLINE difvDeployed #-}

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
difvFieldNames
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DescribeIndexFields
    -> f DescribeIndexFields
difvFieldNames f x =
    (\y -> x { _difvFieldNames = y })
       <$> f (_difvFieldNames x)
{-# INLINE difvFieldNames #-}

instance ToQuery DescribeIndexFields where
    toQuery = genericQuery def

data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse
    { _difwIndexFields :: [IndexFieldStatus]
      -- ^ The index fields configured for the domain.
    } deriving (Show, Generic)

-- | The index fields configured for the domain.
difwIndexFields
    :: Functor f
    => ([IndexFieldStatus]
    -> f ([IndexFieldStatus]))
    -> DescribeIndexFieldsResponse
    -> f DescribeIndexFieldsResponse
difwIndexFields f x =
    (\y -> x { _difwIndexFields = y })
       <$> f (_difwIndexFields x)
{-# INLINE difwIndexFields #-}

instance FromXML DescribeIndexFieldsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeIndexFields where
    type Sv DescribeIndexFields = CloudSearch
    type Rs DescribeIndexFields = DescribeIndexFieldsResponse

    request = post "DescribeIndexFields"
    response _ = xmlResponse
