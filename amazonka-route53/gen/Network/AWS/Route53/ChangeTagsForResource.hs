{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.ChangeTagsForResource
    (
    -- * Request
      ChangeTagsForResource
    -- ** Request constructor
    , changeTagsForResource
    -- ** Request lenses
    , ctfrResourceType
    , ctfrResourceId
    , ctfrTag
    , ctfrKey

    -- * Response
    , ChangeTagsForResourceResponse
    -- ** Response constructor
    , changeTagsForResourceResponse
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type containing information about a request to add, change, or
-- delete the tags that are associated with a resource.
data ChangeTagsForResource = ChangeTagsForResource
    { _ctfrResourceType :: TagResourceType
    , _ctfrResourceId :: Text
    , _ctfrTag :: Maybe (List1 Tag)
    , _ctfrKey :: Maybe (List1 Text)
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeTagsForResource' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ResourceType ::@ @TagResourceType@
--
-- * @ResourceId ::@ @Text@
--
-- * @Tag ::@ @Maybe (List1 Tag)@
--
-- * @Key ::@ @Maybe (List1 Text)@
--
changeTagsForResource :: TagResourceType -- ^ 'ctfrResourceType'
                      -> Text -- ^ 'ctfrResourceId'
                      -> ChangeTagsForResource
changeTagsForResource p1 p2 = ChangeTagsForResource
    { _ctfrResourceType = p1
    , _ctfrResourceId = p2
    , _ctfrTag = Nothing
    , _ctfrKey = Nothing
    }

-- | The type of the resource. The resource type for health checks is
-- healthcheck.
ctfrResourceType :: Lens' ChangeTagsForResource TagResourceType
ctfrResourceType =
    lens _ctfrResourceType (\s a -> s { _ctfrResourceType = a })

-- | The ID of the resource for which you want to add, change, or delete tags.
ctfrResourceId :: Lens' ChangeTagsForResource Text
ctfrResourceId = lens _ctfrResourceId (\s a -> s { _ctfrResourceId = a })

-- | A complex type that contains a list of Tag elements. Each Tag element
-- identifies a tag that you want to add or update for the specified resource.
ctfrTag :: Lens' ChangeTagsForResource (Maybe (List1 Tag))
ctfrTag = lens _ctfrTag (\s a -> s { _ctfrTag = a })

-- | A list of Tag keys that you want to remove from the specified resource.
ctfrKey :: Lens' ChangeTagsForResource (Maybe (List1 Text))
ctfrKey = lens _ctfrKey (\s a -> s { _ctfrKey = a })

instance ToPath ChangeTagsForResource

instance ToQuery ChangeTagsForResource

instance ToHeaders ChangeTagsForResource

instance ToXML ChangeTagsForResource where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ChangeTagsForResource"

-- | Empty response for the request.
data ChangeTagsForResourceResponse = ChangeTagsForResourceResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ChangeTagsForResourceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
changeTagsForResourceResponse :: ChangeTagsForResourceResponse
changeTagsForResourceResponse = ChangeTagsForResourceResponse

instance AWSRequest ChangeTagsForResource where
    type Sv ChangeTagsForResource = Route53
    type Rs ChangeTagsForResource = ChangeTagsForResourceResponse

    request = get
    response _ = nullaryResponse ChangeTagsForResourceResponse
