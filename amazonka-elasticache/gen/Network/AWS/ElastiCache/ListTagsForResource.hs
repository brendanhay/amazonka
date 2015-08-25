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
-- Module      : Network.AWS.ElastiCache.ListTagsForResource
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /ListTagsForResource/ action lists all cost allocation tags
-- currently on the named resource. A /cost allocation tag/ is a key-value
-- pair where the key is case-sensitive and the value is optional. Cost
-- allocation tags can be used to categorize and track your AWS costs.
--
-- You can have a maximum of 10 cost allocation tags on an ElastiCache
-- resource. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/BestPractices.html Using Cost Allocation Tags in Amazon ElastiCache>.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ListTagsForResource.html AWS API Reference> for ListTagsForResource.
module Network.AWS.ElastiCache.ListTagsForResource
    (
    -- * Creating a Request
      listTagsForResource
    , ListTagsForResource
    -- * Request Lenses
    , ltfrResourceName

    -- * Destructuring the Response
    , tagListMessage
    , TagListMessage
    -- * Response Lenses
    , tlmTagList
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input parameters for the /ListTagsForResource/ action.
--
-- /See:/ 'listTagsForResource' smart constructor.
newtype ListTagsForResource = ListTagsForResource'
    { _ltfrResourceName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListTagsForResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltfrResourceName'
listTagsForResource
    :: Text -- ^ 'ltfrResourceName'
    -> ListTagsForResource
listTagsForResource pResourceName_ =
    ListTagsForResource'
    { _ltfrResourceName = pResourceName_
    }

-- | The name of the resource for which you want the list of tags, for
-- example 'arn:aws:elasticache:us-west-2:0123456789:cluster:myCluster'.
ltfrResourceName :: Lens' ListTagsForResource Text
ltfrResourceName = lens _ltfrResourceName (\ s a -> s{_ltfrResourceName = a});

instance AWSRequest ListTagsForResource where
        type Rs ListTagsForResource = TagListMessage
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ListTagsForResourceResult"
              (\ s h x -> parseXML x)

instance ToHeaders ListTagsForResource where
        toHeaders = const mempty

instance ToPath ListTagsForResource where
        toPath = const "/"

instance ToQuery ListTagsForResource where
        toQuery ListTagsForResource'{..}
          = mconcat
              ["Action" =: ("ListTagsForResource" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ResourceName" =: _ltfrResourceName]
