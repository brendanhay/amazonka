{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.CloudSearch.V2013_01_01.DescribeIndexFields where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.CloudSearch.V2013_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'DescribeIndexFields' request.
describeIndexFields :: Text -- ^ '_difrDomainName'
                    -> DescribeIndexFields
describeIndexFields p1 = DescribeIndexFields
    { _difrDomainName = p1
    , _difrDeployed = Nothing
    , _difrFieldNames = mempty
    }

data DescribeIndexFields = DescribeIndexFields
    { _difrDomainName :: Text
      -- ^ The name of the domain you want to describe.
    , _difrDeployed :: Maybe Bool
      -- ^ Whether to display the deployed configuration (true) or include
      -- any pending changes (false). Defaults to false.
    , _difrFieldNames :: [Text]
      -- ^ A list of the index fields you want to describe. If not
      -- specified, information is returned for all configured index
      -- fields.
    } deriving (Generic)

instance ToQuery DescribeIndexFields where
    toQuery = genericToQuery def

instance AWSRequest DescribeIndexFields where
    type Sv DescribeIndexFields = CloudSearch
    type Rs DescribeIndexFields = DescribeIndexFieldsResponse

    request = post "DescribeIndexFields"
    response _ = xmlResponse

data DescribeIndexFieldsResponse = DescribeIndexFieldsResponse
    { _difsIndexFields :: [IndexFieldStatus]
      -- ^ The index fields configured for the domain.
    } deriving (Generic)

instance FromXML DescribeIndexFieldsResponse where
    fromXMLOptions = xmlOptions
