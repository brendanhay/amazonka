{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists all tags on an Amazon RDS resource. For an overview on tagging an
-- Amazon RDS resource, see Tagging Amazon RDS Resources.
module Network.AWS.RDS.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrResourceName

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrTagList
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
newtype ListTagsForResource = ListTagsForResource
    { _ltfrResourceName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResource' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceName ::@ @Text@
--
listTagsForResource :: Text -- ^ 'ltfrResourceName'
                    -> ListTagsForResource
listTagsForResource p1 = ListTagsForResource
    { _ltfrResourceName = p1
    }

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- Constructing an RDS Amazon Resource Name (ARN).
ltfrResourceName :: Lens' ListTagsForResource Text
ltfrResourceName =
    lens _ltfrResourceName (\s a -> s { _ltfrResourceName = a })

instance ToQuery ListTagsForResource where
    toQuery = genericQuery def

-- | 
newtype ListTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrTagList :: [Tag]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListTagsForResourceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TagList ::@ @[Tag]@
--
listTagsForResourceResponse :: ListTagsForResourceResponse
listTagsForResourceResponse = ListTagsForResourceResponse
    { _ltfrrTagList = mempty
    }

-- | List of tags returned by the ListTagsForResource operation.
ltfrrTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrTagList = lens _ltfrrTagList (\s a -> s { _ltfrrTagList = a })

instance FromXML ListTagsForResourceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListTagsForResource where
    type Sv ListTagsForResource = RDS
    type Rs ListTagsForResource = ListTagsForResourceResponse

    request = post "ListTagsForResource"
    response _ = xmlResponse
