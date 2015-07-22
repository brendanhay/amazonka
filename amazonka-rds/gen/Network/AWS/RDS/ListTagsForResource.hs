{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags on an Amazon RDS resource.
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
    , ltfrrqFilters
    , ltfrrqResourceName

    -- * Response
    , ListTagsForResourceResponse
    -- ** Response constructor
    , listTagsForResourceResponse
    -- ** Response lenses
    , ltfrrsTagList
    , ltfrrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'listTagsForResource' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrqFilters'
--
-- * 'ltfrrqResourceName'
data ListTagsForResource = ListTagsForResource'
    { _ltfrrqFilters      :: !(Maybe [Filter])
    , _ltfrrqResourceName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForResource' smart constructor.
listTagsForResource :: Text -> ListTagsForResource
listTagsForResource pResourceName =
    ListTagsForResource'
    { _ltfrrqFilters = Nothing
    , _ltfrrqResourceName = pResourceName
    }

-- | This parameter is not currently supported.
ltfrrqFilters :: Lens' ListTagsForResource [Filter]
ltfrrqFilters = lens _ltfrrqFilters (\ s a -> s{_ltfrrqFilters = a}) . _Default;

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon
-- Resource Name (ARN). For information about creating an ARN, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.html#USER_Tagging.ARN Constructing an RDS Amazon Resource Name (ARN)>.
ltfrrqResourceName :: Lens' ListTagsForResource Text
ltfrrqResourceName = lens _ltfrrqResourceName (\ s a -> s{_ltfrrqResourceName = a});

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
                      may (parseXMLList "Tag"))
                     <*> (pure (fromEnum s)))

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
                 toQuery (toQueryList "Filter" <$> _ltfrrqFilters),
               "ResourceName" =: _ltfrrqResourceName]

-- |
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ltfrrsTagList'
--
-- * 'ltfrrsStatus'
data ListTagsForResourceResponse = ListTagsForResourceResponse'
    { _ltfrrsTagList :: !(Maybe [Tag])
    , _ltfrrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListTagsForResourceResponse' smart constructor.
listTagsForResourceResponse :: Int -> ListTagsForResourceResponse
listTagsForResourceResponse pStatus =
    ListTagsForResourceResponse'
    { _ltfrrsTagList = Nothing
    , _ltfrrsStatus = pStatus
    }

-- | List of tags returned by the ListTagsForResource operation.
ltfrrsTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTagList = lens _ltfrrsTagList (\ s a -> s{_ltfrrsTagList = a}) . _Default;

-- | FIXME: Undocumented member.
ltfrrsStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsStatus = lens _ltfrrsStatus (\ s a -> s{_ltfrrsStatus = a});
