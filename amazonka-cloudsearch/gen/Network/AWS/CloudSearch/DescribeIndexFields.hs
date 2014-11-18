{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudSearch.DescribeIndexFields
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
--
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/API_DescribeIndexFields.html>
module Network.AWS.CloudSearch.DescribeIndexFields
    (
    -- * Request
      DescribeIndexFields
    -- ** Request constructor
    , describeIndexFields
    -- ** Request lenses
    , difDeployed
    , difDomainName
    , difFieldNames

    -- * Response
    , DescribeIndexFieldsResponse
    -- ** Response constructor
    , describeIndexFieldsResponse
    -- ** Response lenses
    , difrIndexFields
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudSearch.Types
import qualified GHC.Exts

data DescribeIndexFields = DescribeIndexFields
    { _difDeployed   :: Maybe Bool
    , _difDomainName :: Text
    , _difFieldNames :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeIndexFields' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difDeployed' @::@ 'Maybe' 'Bool'
--
-- * 'difDomainName' @::@ 'Text'
--
-- * 'difFieldNames' @::@ ['Text']
--
describeIndexFields :: Text -- ^ 'difDomainName'
                    -> DescribeIndexFields
describeIndexFields p1 = DescribeIndexFields
    { _difDomainName = p1
    , _difFieldNames = mempty
    , _difDeployed   = Nothing
    }

-- | Whether to display the deployed configuration (true) or include any
-- pending changes (false). Defaults to false.
difDeployed :: Lens' DescribeIndexFields (Maybe Bool)
difDeployed = lens _difDeployed (\s a -> s { _difDeployed = a })

-- | The name of the domain you want to describe.
difDomainName :: Lens' DescribeIndexFields Text
difDomainName = lens _difDomainName (\s a -> s { _difDomainName = a })

-- | A list of the index fields you want to describe. If not specified,
-- information is returned for all configured index fields.
difFieldNames :: Lens' DescribeIndexFields [Text]
difFieldNames = lens _difFieldNames (\s a -> s { _difFieldNames = a })

newtype DescribeIndexFieldsResponse = DescribeIndexFieldsResponse
    { _difrIndexFields :: [IndexFieldStatus]
    } deriving (Eq, Show, Generic, Monoid, Semigroup)

instance GHC.Exts.IsList DescribeIndexFieldsResponse where
    type Item DescribeIndexFieldsResponse = IndexFieldStatus

    fromList = DescribeIndexFieldsResponse . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _difrIndexFields

-- | 'DescribeIndexFieldsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'difrIndexFields' @::@ ['IndexFieldStatus']
--
describeIndexFieldsResponse :: DescribeIndexFieldsResponse
describeIndexFieldsResponse = DescribeIndexFieldsResponse
    { _difrIndexFields = mempty
    }

-- | The index fields configured for the domain.
difrIndexFields :: Lens' DescribeIndexFieldsResponse [IndexFieldStatus]
difrIndexFields = lens _difrIndexFields (\s a -> s { _difrIndexFields = a })

instance ToPath DescribeIndexFields where
    toPath = const "/"

instance ToQuery DescribeIndexFields

instance ToHeaders DescribeIndexFields

instance AWSRequest DescribeIndexFields where
    type Sv DescribeIndexFields = CloudSearch
    type Rs DescribeIndexFields = DescribeIndexFieldsResponse

    request  = post "DescribeIndexFields"
    response = xmlResponse

instance FromXML DescribeIndexFieldsResponse where
    parseXML x = DescribeIndexFieldsResponse
        <$> x .@ "IndexFields"
