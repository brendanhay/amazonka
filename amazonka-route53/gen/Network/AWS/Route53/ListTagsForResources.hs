{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.ListTagsForResources
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
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTagsForResources.html>
module Network.AWS.Route53.ListTagsForResources
    (
    -- * Request
      ListTagsForResources
    -- ** Request constructor
    , listTagsForResources
    -- ** Request lenses
    , lisResourceType
    , lisResourceIds

    -- * Response
    , ListTagsForResourcesResponse
    -- ** Response constructor
    , listTagsForResourcesResponse
    -- ** Response lenses
    , lisResourceTagSets
    , lisStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types

-- | A complex type containing information about a request for a list of the
-- tags that are associated with up to 10 specified resources.
--
-- /See:/ 'listTagsForResources' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisResourceType'
--
-- * 'lisResourceIds'
data ListTagsForResources = ListTagsForResources'{_lisResourceType :: TagResourceType, _lisResourceIds :: List1 Text} deriving (Eq, Read, Show)

-- | 'ListTagsForResources' smart constructor.
listTagsForResources :: TagResourceType -> NonEmpty Text -> ListTagsForResources
listTagsForResources pResourceType pResourceIds = ListTagsForResources'{_lisResourceType = pResourceType, _lisResourceIds = _List1 # pResourceIds};

-- | The type of the resources.
--
-- - The resource type for health checks is @healthcheck@.
--
-- - The resource type for hosted zones is @hostedzone@.
lisResourceType :: Lens' ListTagsForResources TagResourceType
lisResourceType = lens _lisResourceType (\ s a -> s{_lisResourceType = a});

-- | A complex type that contains the ResourceId element for each resource
-- for which you want to get a list of tags.
lisResourceIds :: Lens' ListTagsForResources (NonEmpty Text)
lisResourceIds = lens _lisResourceIds (\ s a -> s{_lisResourceIds = a}) . _List1;

instance AWSRequest ListTagsForResources where
        type Sv ListTagsForResources = Route53
        type Rs ListTagsForResources =
             ListTagsForResourcesResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 ListTagsForResourcesResponse' <$>
                   (x .@? "ResourceTagSets" .!@ mempty >>=
                      parseXMLList "ResourceTagSet")
                     <*> (pure (fromEnum s)))

instance ToElement ListTagsForResources where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ListTagsForResourcesRequest"

instance ToHeaders ListTagsForResources where
        toHeaders = const mempty

instance ToPath ListTagsForResources where
        toPath ListTagsForResources'{..}
          = mconcat
              ["/2013-04-01/tags/", toText _lisResourceType]

instance ToQuery ListTagsForResources where
        toQuery = const mempty

instance ToXML ListTagsForResources where
        toXML ListTagsForResources'{..}
          = mconcat
              ["ResourceIds" @=
                 toXMLList "ResourceId" _lisResourceIds]

-- | A complex type containing tags for the specified resources.
--
-- /See:/ 'listTagsForResourcesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lisResourceTagSets'
--
-- * 'lisStatusCode'
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'{_lisResourceTagSets :: [ResourceTagSet], _lisStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'ListTagsForResourcesResponse' smart constructor.
listTagsForResourcesResponse :: Int -> ListTagsForResourcesResponse
listTagsForResourcesResponse pStatusCode = ListTagsForResourcesResponse'{_lisResourceTagSets = mempty, _lisStatusCode = pStatusCode};

-- | A list of @ResourceTagSet@s containing tags associated with the
-- specified resources.
lisResourceTagSets :: Lens' ListTagsForResourcesResponse [ResourceTagSet]
lisResourceTagSets = lens _lisResourceTagSets (\ s a -> s{_lisResourceTagSets = a});

-- | FIXME: Undocumented member.
lisStatusCode :: Lens' ListTagsForResourcesResponse Int
lisStatusCode = lens _lisStatusCode (\ s a -> s{_lisStatusCode = a});
