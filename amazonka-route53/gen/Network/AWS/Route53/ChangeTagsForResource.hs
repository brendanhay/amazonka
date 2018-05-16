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
-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds, edits, or deletes tags for a health check or a hosted zone.
--
--
-- For information about using tags for cost allocation, see <http://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Using Cost Allocation Tags> in the /AWS Billing and Cost Management User Guide/ .
--
module Network.AWS.Route53.ChangeTagsForResource
    (
    -- * Creating a Request
      changeTagsForResource
    , ChangeTagsForResource
    -- * Request Lenses
    , ctfrRemoveTagKeys
    , ctfrAddTags
    , ctfrResourceType
    , ctfrResourceId

    -- * Destructuring the Response
    , changeTagsForResourceResponse
    , ChangeTagsForResourceResponse
    -- * Response Lenses
    , ctfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the tags that you want to add, edit, or delete.
--
--
--
-- /See:/ 'changeTagsForResource' smart constructor.
data ChangeTagsForResource = ChangeTagsForResource'
  { _ctfrRemoveTagKeys :: !(Maybe (List1 Text))
  , _ctfrAddTags       :: !(Maybe (List1 Tag))
  , _ctfrResourceType  :: !TagResourceType
  , _ctfrResourceId    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctfrRemoveTagKeys' - A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
--
-- * 'ctfrAddTags' - A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for. You can add a maximum of 10 tags to a health check or a hosted zone.
--
-- * 'ctfrResourceType' - The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
--
-- * 'ctfrResourceId' - The ID of the resource for which you want to add, change, or delete tags.
changeTagsForResource
    :: TagResourceType -- ^ 'ctfrResourceType'
    -> Text -- ^ 'ctfrResourceId'
    -> ChangeTagsForResource
changeTagsForResource pResourceType_ pResourceId_ =
  ChangeTagsForResource'
    { _ctfrRemoveTagKeys = Nothing
    , _ctfrAddTags = Nothing
    , _ctfrResourceType = pResourceType_
    , _ctfrResourceId = pResourceId_
    }


-- | A complex type that contains a list of the tags that you want to delete from the specified health check or hosted zone. You can specify up to 10 keys.
ctfrRemoveTagKeys :: Lens' ChangeTagsForResource (Maybe (NonEmpty Text))
ctfrRemoveTagKeys = lens _ctfrRemoveTagKeys (\ s a -> s{_ctfrRemoveTagKeys = a}) . mapping _List1

-- | A complex type that contains a list of the tags that you want to add to the specified health check or hosted zone and/or the tags that you want to edit @Value@ for. You can add a maximum of 10 tags to a health check or a hosted zone.
ctfrAddTags :: Lens' ChangeTagsForResource (Maybe (NonEmpty Tag))
ctfrAddTags = lens _ctfrAddTags (\ s a -> s{_ctfrAddTags = a}) . mapping _List1

-- | The type of the resource.     * The resource type for health checks is @healthcheck@ .     * The resource type for hosted zones is @hostedzone@ .
ctfrResourceType :: Lens' ChangeTagsForResource TagResourceType
ctfrResourceType = lens _ctfrResourceType (\ s a -> s{_ctfrResourceType = a})

-- | The ID of the resource for which you want to add, change, or delete tags.
ctfrResourceId :: Lens' ChangeTagsForResource Text
ctfrResourceId = lens _ctfrResourceId (\ s a -> s{_ctfrResourceId = a})

instance AWSRequest ChangeTagsForResource where
        type Rs ChangeTagsForResource =
             ChangeTagsForResourceResponse
        request = postXML route53
        response
          = receiveEmpty
              (\ s h x ->
                 ChangeTagsForResourceResponse' <$>
                   (pure (fromEnum s)))

instance Hashable ChangeTagsForResource where

instance NFData ChangeTagsForResource where

instance ToElement ChangeTagsForResource where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance ToHeaders ChangeTagsForResource where
        toHeaders = const mempty

instance ToPath ChangeTagsForResource where
        toPath ChangeTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toBS _ctfrResourceType, "/",
               toBS _ctfrResourceId]

instance ToQuery ChangeTagsForResource where
        toQuery = const mempty

instance ToXML ChangeTagsForResource where
        toXML ChangeTagsForResource'{..}
          = mconcat
              ["RemoveTagKeys" @=
                 toXML (toXMLList "Key" <$> _ctfrRemoveTagKeys),
               "AddTags" @=
                 toXML (toXMLList "Tag" <$> _ctfrAddTags)]

-- | Empty response for the request.
--
--
--
-- /See:/ 'changeTagsForResourceResponse' smart constructor.
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse'
  { _ctfrrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ChangeTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctfrrsResponseStatus' - -- | The response status code.
changeTagsForResourceResponse
    :: Int -- ^ 'ctfrrsResponseStatus'
    -> ChangeTagsForResourceResponse
changeTagsForResourceResponse pResponseStatus_ =
  ChangeTagsForResourceResponse' {_ctfrrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ctfrrsResponseStatus :: Lens' ChangeTagsForResourceResponse Int
ctfrrsResponseStatus = lens _ctfrrsResponseStatus (\ s a -> s{_ctfrrsResponseStatus = a})

instance NFData ChangeTagsForResourceResponse where
