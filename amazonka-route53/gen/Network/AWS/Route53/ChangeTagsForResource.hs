{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.ChangeTagsForResource
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
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeTagsForResource.html>
module Network.AWS.Route53.ChangeTagsForResource
    (
    -- * Request
      ChangeTagsForResource
    -- ** Request constructor
    , changeTagsForResource
    -- ** Request lenses
    , ctfrResourceType
    , ctfrResourceId
    , ctfrRemoveTagKeys
    , ctfrAddTags

    -- * Response
    , ChangeTagsForResourceResponse
    -- ** Response constructor
    , changeTagsForResourceResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'changeTagsForResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfrResourceType'
--
-- * 'ctfrResourceId'
--
-- * 'ctfrRemoveTagKeys'
--
-- * 'ctfrAddTags'
data ChangeTagsForResource = ChangeTagsForResource'{_ctfrResourceType :: TagResourceType, _ctfrResourceId :: Text, _ctfrRemoveTagKeys :: List1 Text, _ctfrAddTags :: List1 Tag} deriving (Eq, Read, Show)

-- | 'ChangeTagsForResource' smart constructor.
changeTagsForResource :: TagResourceType -> Text -> NonEmpty Text -> NonEmpty Tag -> ChangeTagsForResource
changeTagsForResource pResourceType pResourceId pRemoveTagKeys pAddTags = ChangeTagsForResource'{_ctfrResourceType = pResourceType, _ctfrResourceId = pResourceId, _ctfrRemoveTagKeys = _List1 # pRemoveTagKeys, _ctfrAddTags = _List1 # pAddTags};

-- | The type of the resource.
--
-- - The resource type for health checks is @healthcheck@.
--
-- - The resource type for hosted zones is @hostedzone@.
ctfrResourceType :: Lens' ChangeTagsForResource TagResourceType
ctfrResourceType = lens _ctfrResourceType (\ s a -> s{_ctfrResourceType = a});

-- | The ID of the resource for which you want to add, change, or delete
-- tags.
ctfrResourceId :: Lens' ChangeTagsForResource Text
ctfrResourceId = lens _ctfrResourceId (\ s a -> s{_ctfrResourceId = a});

-- | A list of @Tag@ keys that you want to remove from the specified
-- resource.
ctfrRemoveTagKeys :: Lens' ChangeTagsForResource (NonEmpty Text)
ctfrRemoveTagKeys = lens _ctfrRemoveTagKeys (\ s a -> s{_ctfrRemoveTagKeys = a}) . _List1;

-- | A complex type that contains a list of @Tag@ elements. Each @Tag@
-- element identifies a tag that you want to add or update for the
-- specified resource.
ctfrAddTags :: Lens' ChangeTagsForResource (NonEmpty Tag)
ctfrAddTags = lens _ctfrAddTags (\ s a -> s{_ctfrAddTags = a}) . _List1;

instance AWSRequest ChangeTagsForResource where
        type Sv ChangeTagsForResource = Route53
        type Rs ChangeTagsForResource =
             ChangeTagsForResourceResponse
        request = postXML
        response = receiveNull ChangeTagsForResourceResponse'

instance ToElement ChangeTagsForResource where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance ToHeaders ChangeTagsForResource where
        toHeaders = const mempty

instance ToPath ChangeTagsForResource where
        toPath ChangeTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toText _ctfrResourceType, "/",
               toText _ctfrResourceId]

instance ToQuery ChangeTagsForResource where
        toQuery = const mempty

instance ToXML ChangeTagsForResource where
        toXML ChangeTagsForResource'{..}
          = mconcat
              ["RemoveTagKeys" @= "Key" @@= _ctfrRemoveTagKeys,
               "AddTags" @= "Tag" @@= _ctfrAddTags]

-- | /See:/ 'changeTagsForResourceResponse' smart constructor.
data ChangeTagsForResourceResponse = ChangeTagsForResourceResponse' deriving (Eq, Read, Show)

-- | 'ChangeTagsForResourceResponse' smart constructor.
changeTagsForResourceResponse :: ChangeTagsForResourceResponse
changeTagsForResourceResponse = ChangeTagsForResourceResponse';
