{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- FIXME: Undocumented operation.
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
    , ltfrrsStatus
    , ltfrrsResourceTagSet
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForResource' smart constructor.
listTagsForResource :: TagResourceType -> Text -> ListTagsForResource
listTagsForResource pResourceType_ pResourceId_ =
    ListTagsForResource'
    { _ltfrResourceType = pResourceType_
    , _ltfrResourceId = pResourceId_
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
                   (pure (fromEnum s)) <*> (x .@ "ResourceTagSet"))

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath ListTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toPath _ltfrResourceType, "/",
               toPath _ltfrResourceId]

instance ToQuery ListTagsForResource where
        toQuery = const mempty

-- | A complex type containing tags for the specified resource.
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrsStatus'
--
-- * 'ltfrrsResourceTagSet'
data ListTagsForResourceResponse = ListTagsForResourceResponse'
    { _ltfrrsStatus         :: !Int
    , _ltfrrsResourceTagSet :: !ResourceTagSet
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForResourceResponse' smart constructor.
listTagsForResourceResponse :: Int -> ResourceTagSet -> ListTagsForResourceResponse
listTagsForResourceResponse pStatus_ pResourceTagSet_ =
    ListTagsForResourceResponse'
    { _ltfrrsStatus = pStatus_
    , _ltfrrsResourceTagSet = pResourceTagSet_
    }

-- | FIXME: Undocumented member.
ltfrrsStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsStatus = lens _ltfrrsStatus (\ s a -> s{_ltfrrsStatus = a});

-- | A @ResourceTagSet@ containing tags associated with the specified
-- resource.
ltfrrsResourceTagSet :: Lens' ListTagsForResourceResponse ResourceTagSet
ltfrrsResourceTagSet = lens _ltfrrsResourceTagSet (\ s a -> s{_ltfrrsResourceTagSet = a});
