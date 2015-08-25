{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResources
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Undocumented operation.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListTagsForResources.html AWS API Reference> for ListTagsForResources.
module Network.AWS.Route53.ListTagsForResources
    (
    -- * Creating a Request
      listTagsForResources
    , ListTagsForResources
    -- * Request Lenses
    , lResourceType
    , lResourceIds

    -- * Destructuring the Response
    , listTagsForResourcesResponse
    , ListTagsForResourcesResponse
    -- * Response Lenses
    , lrsStatus
    , lrsResourceTagSets
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type containing information about a request for a list of the
-- tags that are associated with up to 10 specified resources.
--
-- /See:/ 'listTagsForResources' smart constructor.
data ListTagsForResources = ListTagsForResources'
    { _lResourceType :: !TagResourceType
    , _lResourceIds  :: !(List1 Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTagsForResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lResourceType'
--
-- * 'lResourceIds'
listTagsForResources
    :: TagResourceType -- ^ 'lResourceType'
    -> NonEmpty Text -- ^ 'lResourceIds'
    -> ListTagsForResources
listTagsForResources pResourceType_ pResourceIds_ =
    ListTagsForResources'
    { _lResourceType = pResourceType_
    , _lResourceIds = _List1 # pResourceIds_
    }

-- | The type of the resources.
--
-- - The resource type for health checks is 'healthcheck'.
--
-- - The resource type for hosted zones is 'hostedzone'.
lResourceType :: Lens' ListTagsForResources TagResourceType
lResourceType = lens _lResourceType (\ s a -> s{_lResourceType = a});

-- | A complex type that contains the ResourceId element for each resource
-- for which you want to get a list of tags.
lResourceIds :: Lens' ListTagsForResources (NonEmpty Text)
lResourceIds = lens _lResourceIds (\ s a -> s{_lResourceIds = a}) . _List1;

instance AWSRequest ListTagsForResources where
        type Rs ListTagsForResources =
             ListTagsForResourcesResponse
        request = postXML route53
        response
          = receiveXML
              (\ s h x ->
                 ListTagsForResourcesResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "ResourceTagSets" .!@ mempty >>=
                        parseXMLList "ResourceTagSet"))

instance ToElement ListTagsForResources where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ListTagsForResourcesRequest"

instance ToHeaders ListTagsForResources where
        toHeaders = const mempty

instance ToPath ListTagsForResources where
        toPath ListTagsForResources'{..}
          = mconcat ["/2013-04-01/tags/", toBS _lResourceType]

instance ToQuery ListTagsForResources where
        toQuery = const mempty

instance ToXML ListTagsForResources where
        toXML ListTagsForResources'{..}
          = mconcat
              ["ResourceIds" @=
                 toXMLList "ResourceId" _lResourceIds]

-- | A complex type containing tags for the specified resources.
--
-- /See:/ 'listTagsForResourcesResponse' smart constructor.
data ListTagsForResourcesResponse = ListTagsForResourcesResponse'
    { _lrsStatus          :: !Int
    , _lrsResourceTagSets :: ![ResourceTagSet]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTagsForResourcesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsStatus'
--
-- * 'lrsResourceTagSets'
listTagsForResourcesResponse
    :: Int -- ^ 'lrsStatus'
    -> ListTagsForResourcesResponse
listTagsForResourcesResponse pStatus_ =
    ListTagsForResourcesResponse'
    { _lrsStatus = pStatus_
    , _lrsResourceTagSets = mempty
    }

-- | The response status code.
lrsStatus :: Lens' ListTagsForResourcesResponse Int
lrsStatus = lens _lrsStatus (\ s a -> s{_lrsStatus = a});

-- | A list of 'ResourceTagSet's containing tags associated with the
-- specified resources.
lrsResourceTagSets :: Lens' ListTagsForResourcesResponse [ResourceTagSet]
lrsResourceTagSets = lens _lrsResourceTagSets (\ s a -> s{_lrsResourceTagSets = a}) . _Coerce;
