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
    , mkDescribeIndexFields
    -- ** Request lenses
    , dif2DomainName
    , dif2FieldNames
    , dif2Deployed

    -- * Response
    , DescribeIndexFieldsResponse
    -- ** Response lenses
    , difrs1IndexFields
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudSearch.V2013_01_01.Types
import Network.AWS.Prelude

-- | Container for the parameters to the DescribeIndexFields operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular index fields, specify the names of the index fields
-- you want to describe. To show the active configuration and exclude any
-- pending changes, set the Deployed option to true.
data DescribeIndexFields = DescribeIndexFields
    { _dif2DomainName :: Text
    , _dif2FieldNames :: [Text]
    , _dif2Deployed :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeIndexFields' request.
mkDescribeIndexFields :: Text -- ^ 'dif2DomainName'
                      -> DescribeIndexFields
mkDescribeIndexFields p1 = DescribeIndexFields
    { _dif2DomainName = p1
    , _dif2FieldNames = mempty
    , _dif2Deployed = Nothing
    }

-- | The name of the domain you want to describe.
dif2DomainName :: Lens' DescribeIndexFields Text
dif2DomainName = lens _dif2DomainName (\s a -> s { _dif2DomainName = a })

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
dif2FieldNames :: Lens' DescribeIndexFields [Text]
dif2FieldNames = lens _dif2FieldNames (\s a -> s { _dif2FieldNames = a })

-- | Whether to display the deployed configuration (true) or include any pending
-- changes (false). Defaults to false.
dif2Deployed :: Lens' DescribeIndexFields (Maybe Bool)
dif2Deployed = lens _dif2Deployed (\s a -> s { _dif2Deployed = a })

instance ToQuery DescribeIndexFields where
    toQuery = genericQuery def

-- | The result of a DescribeIndexFields request. Contains the index fields
-- configured for the domain specified in the request.
newtype DescribeIndexFieldsResponse = DescribeIndexFieldsResponse
    { _difrs1IndexFields :: [IndexFieldStatus]
    } deriving (Show, Generic)

-- | The index fields configured for the domain.
difrs1IndexFields :: Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difrs1IndexFields =
    lens _difrs1IndexFields (\s a -> s { _difrs1IndexFields = a })

instance FromXML DescribeIndexFieldsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeIndexFields where
    type Sv DescribeIndexFields = CloudSearch
    type Rs DescribeIndexFields = DescribeIndexFieldsResponse

    request = post "DescribeIndexFields"
    response _ = xmlResponse
