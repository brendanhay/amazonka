{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ChangeTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- FIXME: Undocumented operation.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ChangeTagsForResource.html>
module Network.AWS.Route53.ChangeTagsForResource
    (
    -- * Request
      ChangeTagsForResource
    -- ** Request constructor
    , changeTagsForResource
    -- ** Request lenses
    , ctfrrqRemoveTagKeys
    , ctfrrqAddTags
    , ctfrrqResourceType
    , ctfrrqResourceId

    -- * Response
    , ChangeTagsForResourceResponse
    -- ** Response constructor
    , changeTagsForResourceResponse
    -- ** Response lenses
    , ctfrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type containing information about a request to add, change, or
-- delete the tags that are associated with a resource.
--
-- /See:/ 'changeTagsForResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfrrqRemoveTagKeys'
--
-- * 'ctfrrqAddTags'
--
-- * 'ctfrrqResourceType'
--
-- * 'ctfrrqResourceId'
data ChangeTagsForResource = ChangeTagsForResource'
    { _ctfrrqRemoveTagKeys :: !(Maybe (List1 Text))
    , _ctfrrqAddTags       :: !(Maybe (List1 Tag))
    , _ctfrrqResourceType  :: !TagResourceType
    , _ctfrrqResourceId    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangeTagsForResource' smart constructor.
changeTagsForResource :: TagResourceType -> Text -> ChangeTagsForResource
changeTagsForResource pResourceType pResourceId =
    ChangeTagsForResource'
    { _ctfrrqRemoveTagKeys = Nothing
    , _ctfrrqAddTags = Nothing
    , _ctfrrqResourceType = pResourceType
    , _ctfrrqResourceId = pResourceId
    }

-- | A list of @Tag@ keys that you want to remove from the specified
-- resource.
ctfrrqRemoveTagKeys :: Lens' ChangeTagsForResource (Maybe (NonEmpty Text))
ctfrrqRemoveTagKeys = lens _ctfrrqRemoveTagKeys (\ s a -> s{_ctfrrqRemoveTagKeys = a}) . mapping _List1;

-- | A complex type that contains a list of @Tag@ elements. Each @Tag@
-- element identifies a tag that you want to add or update for the
-- specified resource.
ctfrrqAddTags :: Lens' ChangeTagsForResource (Maybe (NonEmpty Tag))
ctfrrqAddTags = lens _ctfrrqAddTags (\ s a -> s{_ctfrrqAddTags = a}) . mapping _List1;

-- | The type of the resource.
--
-- - The resource type for health checks is @healthcheck@.
--
-- - The resource type for hosted zones is @hostedzone@.
ctfrrqResourceType :: Lens' ChangeTagsForResource TagResourceType
ctfrrqResourceType = lens _ctfrrqResourceType (\ s a -> s{_ctfrrqResourceType = a});

-- | The ID of the resource for which you want to add, change, or delete
-- tags.
ctfrrqResourceId :: Lens' ChangeTagsForResource Text
ctfrrqResourceId = lens _ctfrrqResourceId (\ s a -> s{_ctfrrqResourceId = a});

instance AWSRequest ChangeTagsForResource where
        type Sv ChangeTagsForResource = Route53
        type Rs ChangeTagsForResource =
             ChangeTagsForResourceResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 ChangeTagsForResourceResponse' <$>
                   (pure (fromEnum s)))

instance ToElement ChangeTagsForResource where
        toElement
          = mkElement
              "{https://route53.amazonaws.com/doc/2013-04-01/}ChangeTagsForResourceRequest"

instance ToHeaders ChangeTagsForResource where
        toHeaders = const mempty

instance ToPath ChangeTagsForResource where
        toPath ChangeTagsForResource'{..}
          = mconcat
              ["/2013-04-01/tags/", toText _ctfrrqResourceType,
               "/", toText _ctfrrqResourceId]

instance ToQuery ChangeTagsForResource where
        toQuery = const mempty

instance ToXML ChangeTagsForResource where
        toXML ChangeTagsForResource'{..}
          = mconcat
              ["RemoveTagKeys" @=
                 toXML (toXMLList "Key" <$> _ctfrrqRemoveTagKeys),
               "AddTags" @=
                 toXML (toXMLList "Tag" <$> _ctfrrqAddTags)]

-- | Empty response for the request.
--
-- /See:/ 'changeTagsForResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ctfrrsStatus'
newtype ChangeTagsForResourceResponse = ChangeTagsForResourceResponse'
    { _ctfrrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ChangeTagsForResourceResponse' smart constructor.
changeTagsForResourceResponse :: Int -> ChangeTagsForResourceResponse
changeTagsForResourceResponse pStatus =
    ChangeTagsForResourceResponse'
    { _ctfrrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ctfrrsStatus :: Lens' ChangeTagsForResourceResponse Int
ctfrrsStatus = lens _ctfrrsStatus (\ s a -> s{_ctfrrsStatus = a});
