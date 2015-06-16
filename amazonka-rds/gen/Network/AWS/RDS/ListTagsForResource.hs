{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.ListTagsForResource
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

-- | Lists all tags on an Amazon RDS resource.
--
-- For an overview on tagging an Amazon RDS resource, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources>.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_ListTagsForResource.html>
module Network.AWS.RDS.ListTagsForResource
    (
    -- * Request
      ListTagsForResource
    -- ** Request constructor
    , listTagsForResource
    -- ** Request lenses
    , ltfrFilters
    , ltfrResourceName

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrTagList
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.RDS.Types

-- | /See:/ 'listTagsForResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrFilters'
--
-- * 'ltfrResourceName'
data ListTagsForResource = ListTagsForResource'{_ltfrFilters :: Maybe [Filter], _ltfrResourceName :: Text} deriving (Eq, Read, Show)

-- | 'ListTagsForResource' smart constructor.
listTagsForResource :: Text -> ListTagsForResource
listTagsForResource pResourceName = ListTagsForResource'{_ltfrFilters = Nothing, _ltfrResourceName = pResourceName};

-- | This parameter is not currently supported.
ltfrFilters :: Lens' ListTagsForResource [Filter]
ltfrFilters = lens _ltfrFilters (\ s a -> s{_ltfrFilters = a}) . _Default;

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
ltfrResourceName :: Lens' ListTagsForResource Text
ltfrResourceName = lens _ltfrResourceName (\ s a -> s{_ltfrResourceName = a});

instance AWSRequest ListTagsForResource where
        type Sv ListTagsForResource = RDS
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = post
        response
          = receiveXMLWrapper "ListTagsForResourceResult"
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .@? "TagList" .!@ mempty >>=
                      may (parseXMLList "Tag")))

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery ListTagsForResource'{..}
          = mconcat
              ["Action" =: ("ListTagsForResource" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Filters" =:
                 toQuery (toQueryList "Filter" <$> _ltfrFilters),
               "ResourceName" =: _ltfrResourceName]

-- | /See:/ 'listTagsForResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrTagList'
newtype ListTagsForResourceResponse = ListTagsForResourceResponse'{_ltfrrTagList :: Maybe [Tag]} deriving (Eq, Read, Show)

-- | 'ListTagsForResourceResponse' smart constructor.
listTagsForResourceResponse :: ListTagsForResourceResponse
listTagsForResourceResponse = ListTagsForResourceResponse'{_ltfrrTagList = Nothing};

-- | List of tags returned by the ListTagsForResource operation.
ltfrrTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrTagList = lens _ltfrrTagList (\ s a -> s{_ltfrrTagList = a}) . _Default;
