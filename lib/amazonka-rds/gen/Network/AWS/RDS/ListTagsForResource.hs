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
-- Module      : Network.AWS.RDS.ListTagsForResource
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all tags on an Amazon RDS resource.
--
--
-- For an overview on tagging an Amazon RDS resource, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Overview.Tagging.html Tagging Amazon RDS Resources> .
--
module Network.AWS.RDS.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrFilters
    , ltfrResourceName

    -- * Destructuring the Response
    , listTagsForResourceResponse
    , ListTagsForResourceResponse
    -- * Response Lenses
    , ltfrrsTagList
    , ltfrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'listTagsForResource' smart constructor.
data ListTagsForResource = ListTagsForResource'
  { _ltfrFilters      :: !(Maybe [Filter])
  , _ltfrResourceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrFilters' - This parameter is not currently supported.
--
-- * 'ltfrResourceName' - The Amazon RDS resource with tags to be listed. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
listTagsForResource
    :: Text -- ^ 'ltfrResourceName'
    -> ListTagsForResource
listTagsForResource pResourceName_ =
  ListTagsForResource'
    {_ltfrFilters = Nothing, _ltfrResourceName = pResourceName_}


-- | This parameter is not currently supported.
ltfrFilters :: Lens' ListTagsForResource [Filter]
ltfrFilters = lens _ltfrFilters (\ s a -> s{_ltfrFilters = a}) . _Default . _Coerce

-- | The Amazon RDS resource with tags to be listed. This value is an Amazon Resource Name (ARN). For information about creating an ARN, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
ltfrResourceName :: Lens' ListTagsForResource Text
ltfrResourceName = lens _ltfrResourceName (\ s a -> s{_ltfrResourceName = a})

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource =
             ListTagsForResourceResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ListTagsForResourceResult"
              (\ s h x ->
                 ListTagsForResourceResponse' <$>
                   (x .@? "TagList" .!@ mempty >>=
                      may (parseXMLList "Tag"))
                     <*> (pure (fromEnum s)))

instance Hashable ListTagsForResource where

instance NFData ListTagsForResource where

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

-- |
--
--
--
-- /See:/ 'listTagsForResourceResponse' smart constructor.
data ListTagsForResourceResponse = ListTagsForResourceResponse'
  { _ltfrrsTagList        :: !(Maybe [Tag])
  , _ltfrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTagsForResourceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrrsTagList' - List of tags returned by the ListTagsForResource operation.
--
-- * 'ltfrrsResponseStatus' - -- | The response status code.
listTagsForResourceResponse
    :: Int -- ^ 'ltfrrsResponseStatus'
    -> ListTagsForResourceResponse
listTagsForResourceResponse pResponseStatus_ =
  ListTagsForResourceResponse'
    {_ltfrrsTagList = Nothing, _ltfrrsResponseStatus = pResponseStatus_}


-- | List of tags returned by the ListTagsForResource operation.
ltfrrsTagList :: Lens' ListTagsForResourceResponse [Tag]
ltfrrsTagList = lens _ltfrrsTagList (\ s a -> s{_ltfrrsTagList = a}) . _Default . _Coerce

-- | -- | The response status code.
ltfrrsResponseStatus :: Lens' ListTagsForResourceResponse Int
ltfrrsResponseStatus = lens _ltfrrsResponseStatus (\ s a -> s{_ltfrrsResponseStatus = a})

instance NFData ListTagsForResourceResponse where
