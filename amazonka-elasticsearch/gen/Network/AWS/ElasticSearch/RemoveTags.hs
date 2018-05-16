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
-- Module      : Network.AWS.ElasticSearch.RemoveTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified set of tags from the specified Elasticsearch domain.
--
--
module Network.AWS.ElasticSearch.RemoveTags
    (
    -- * Creating a Request
      removeTags
    , RemoveTags
    -- * Request Lenses
    , rtARN
    , rtTagKeys

    -- * Destructuring the Response
    , removeTagsResponse
    , RemoveTagsResponse
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'RemoveTags' @ operation. Specify the @ARN@ for the Elasticsearch domain from which you want to remove the specified @TagKey@ .
--
--
--
-- /See:/ 'removeTags' smart constructor.
data RemoveTags = RemoveTags'
  { _rtARN     :: !Text
  , _rtTagKeys :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtARN' - Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
--
-- * 'rtTagKeys' - Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
removeTags
    :: Text -- ^ 'rtARN'
    -> RemoveTags
removeTags pARN_ = RemoveTags' {_rtARN = pARN_, _rtTagKeys = mempty}


-- | Specifies the @ARN@ for the Elasticsearch domain from which you want to delete the specified tags.
rtARN :: Lens' RemoveTags Text
rtARN = lens _rtARN (\ s a -> s{_rtARN = a})

-- | Specifies the @TagKey@ list which you want to remove from the Elasticsearch domain.
rtTagKeys :: Lens' RemoveTags [Text]
rtTagKeys = lens _rtTagKeys (\ s a -> s{_rtTagKeys = a}) . _Coerce

instance AWSRequest RemoveTags where
        type Rs RemoveTags = RemoveTagsResponse
        request = postJSON elasticSearch
        response = receiveNull RemoveTagsResponse'

instance Hashable RemoveTags where

instance NFData RemoveTags where

instance ToHeaders RemoveTags where
        toHeaders = const mempty

instance ToJSON RemoveTags where
        toJSON RemoveTags'{..}
          = object
              (catMaybes
                 [Just ("ARN" .= _rtARN),
                  Just ("TagKeys" .= _rtTagKeys)])

instance ToPath RemoveTags where
        toPath = const "/2015-01-01/tags-removal"

instance ToQuery RemoveTags where
        toQuery = const mempty

-- | /See:/ 'removeTagsResponse' smart constructor.
data RemoveTagsResponse =
  RemoveTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RemoveTagsResponse' with the minimum fields required to make a request.
--
removeTagsResponse
    :: RemoveTagsResponse
removeTagsResponse = RemoveTagsResponse'


instance NFData RemoveTagsResponse where
