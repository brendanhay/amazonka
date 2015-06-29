{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | FIXME: Undocumented operation.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTagsForResource.html>
module Network.AWS.Route53.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrResourceType
    , ltfrResourceId

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrStatus
    , ltfrrResourceTagSet
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type containing information about a request for a list of the
-- tags that are associated with an individual resource.
--
-- /See:/ 'listTagsForResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrResourceType'
--
-- * 'ltfrResourceId'
data ListTagsForResource = ListTagsForResource'
    { _ltfrResourceType :: !TagResourceType
    , _ltfrResourceId   :: !Text
    } deriving (Eq,Read,Show)

-- | 'ListTagsForResource' smart constructor.
listTagsForResource :: TagResourceType -> Text -> ListTagsForResource
listTagsForResource pResourceType pResourceId =
    ListTagsForResource'
    { _ltfrResourceType = pResourceType
    , _ltfrResourceId = pResourceId
    }

-- | The type of the resource.
--
-- - The resource type for health checks is @healthcheck@.
--
-- - The resource type for hosted zones is @hostedzone@.
ltfrResourceType :: Lens' ListTagsForResource TagResourceType
ltfrResourceType = lens _ltfrResourceType (\ s a -> s{_ltfrResourceType = a});

-- | The ID of the resource for which you want to retrieve tags.
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\ s a -> s{_ltfrResourceId = a});

instance AWSRequest ListTagsForResource where
        type Sv ListTagsForResource = Route53
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (pure s) <*> (x .@ "ResourceTagSet"))

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath ListTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toText _ltfrResourceType, "/",
               toText _ltfrResourceId]

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | A complex type containing tags for the specified resource.
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrStatus'
--
-- * 'ltfrrResourceTagSet'
data ListTagsForResourceResponse = ListTagsForResourceResponse'
    { _ltfrrStatus         :: !Status
    , _ltfrrResourceTagSet :: !ResourceTagSet
    } deriving (Eq,Show)

-- | 'ListTagsForResourceResponse' smart constructor.
listTagsForResourceResponse :: Status -> ResourceTagSet -> ListTagsForResourceResponse
listTagsForResourceResponse pStatus pResourceTagSet =
    ListTagsForResourceResponse'
    { _ltfrrStatus = pStatus
    , _ltfrrResourceTagSet = pResourceTagSet
    }

-- | FIXME: Undocumented member.
ltfrrStatus :: Lens' ListTagsForResourceResponse Status
ltfrrStatus = lens _ltfrrStatus (\ s a -> s{_ltfrrStatus = a});

-- | A @ResourceTagSet@ containing tags associated with the specified
-- resource.
ltfrrResourceTagSet :: Lens' ListTagsForResourceResponse ResourceTagSet
ltfrrResourceTagSet = lens _ltfrrResourceTagSet (\ s a -> s{_ltfrrResourceTagSet = a});
