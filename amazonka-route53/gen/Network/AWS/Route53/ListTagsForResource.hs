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
-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists tags for one health check or hosted zone.
--
--
-- For information about using tags for cost allocation, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
module Network.AWS.Route53.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrResourceType
    , ltfrResourceId

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsResponseStatus
    , ltfrrsResourceTagSet
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type containing information about a request for a list of the tags that are associated with an individual resource.
--
--
--
-- /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { _ltfrResourceType :: !TagResourceType
  , _ltfrResourceId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrResourceType' - The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
--
-- * 'ltfrResourceId' - The ID of the resource for which you want to retrieve tags.
listTagsForResource
    :: TagResourceType -- ^ 'ltfrResourceType'
    -> Text -- ^ 'ltfrResourceId'
    -> ListTagsForResource
listTagsForResource pResourceType_ pResourceId_ =
  ListTagsForResource'
    {_ltfrResourceType = pResourceType_, _ltfrResourceId = pResourceId_}


-- | The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
ltfrResourceType :: Lens' ListTagsForResource TagResourceType
ltfrResourceType = lens _ltfrResourceType (\ s a -> s{_ltfrResourceType = a})

-- | The ID of the resource for which you want to retrieve tags.
ltfrResourceId :: Lens' ListTagsForResource Text
ltfrResourceId = lens _ltfrResourceId (\ s a -> s{_ltfrResourceId = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "ResourceTagSet"))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath ListTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toBS _ltfrResourceType, "/",
               toBS _ltfrResourceId]

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | A complex type that contains information about the health checks or hosted zones for which you want to list tags.
--
--
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsResponseStatus :: !Int
  , _ltfrrsResourceTagSet :: !ResourceTagSet
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
--
-- * 'ltfrrsResourceTagSet' - A @ResourceTagSet@ containing tags associated with the specified resource.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ResourceTagSet -- ^ 'ltfrrsResourceTagSet'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ pResourceTagSet_ =
  ListTagsForResourceResponse'
    { _ltfrrsResponseStatus = pResponseStatus_
    , _ltfrrsResourceTagSet = pResourceTagSet_
    }


-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

-- | A @ResourceTagSet@ containing tags associated with the specified resource.
ltfrrsResourceTagSet :: Lens' ListTagsForResourceResponse ResourceTagSet
ltfrrsResourceTagSet = lens _ltfrrsResourceTagSet (\ s a -> s{_ltfrrsResourceTagSet = a})

instance NFData ListTagsForResourceResponse where
