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
-- Module      : Network.AWS.ElasticSearch.AddTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches tags to an existing Elasticsearch domain. Tags are a set of case-sensitive key value pairs. An Elasticsearch domain may have up to 10 tags. See <http://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/es-managedomains.html#es-managedomains-awsresorcetagging Tagging Amazon Elasticsearch Service Domains for more information.>
--
--
module Network.AWS.ElasticSearch.AddTags
    (
    -- * Creating a Request
      addTags
    , AddTags
    -- * Request Lenses
    , atARN
    , atTagList

    -- * Destructuring the Response
    , addTagsResponse
    , AddTagsResponse
    ) where

import Network.AWS.ElasticSearch.Types
import Network.AWS.ElasticSearch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'AddTags' @ operation. Specify the tags that you want to attach to the Elasticsearch domain.
--
--
--
-- /See:/ 'addTags' smart constructor.
data AddTags = AddTags'
  { _atARN     :: !Text
  , _atTagList :: ![Tag]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'atARN' - Specify the @ARN@ for which you want to add the tags.
--
-- * 'atTagList' - List of @Tag@ that need to be added for the Elasticsearch domain.
addTags
    :: Text -- ^ 'atARN'
    -> AddTags
addTags pARN_ = AddTags' {_atARN = pARN_, _atTagList = mempty}


-- | Specify the @ARN@ for which you want to add the tags.
atARN :: Lens' AddTags Text
atARN = lens _atARN (\ s a -> s{_atARN = a})

-- | List of @Tag@ that need to be added for the Elasticsearch domain.
atTagList :: Lens' AddTags [Tag]
atTagList = lens _atTagList (\ s a -> s{_atTagList = a}) . _Coerce

instance AWSRequest AddTags where
        type Rs AddTags = AddTagsResponse
        request = postJSON elasticSearch
        response = receiveNull AddTagsResponse'

instance Hashable AddTags where

instance NFData AddTags where

instance ToHeaders AddTags where
        toHeaders = const mempty

instance ToJSON AddTags where
        toJSON AddTags'{..}
          = object
              (catMaybes
                 [Just ("ARN" .= _atARN),
                  Just ("TagList" .= _atTagList)])

instance ToPath AddTags where
        toPath = const "/2015-01-01/tags"

instance ToQuery AddTags where
        toQuery = const mempty

-- | /See:/ 'addTagsResponse' smart constructor.
data AddTagsResponse =
  AddTagsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddTagsResponse' with the minimum fields required to make a request.
--
addTagsResponse
    :: AddTagsResponse
addTagsResponse = AddTagsResponse'


instance NFData AddTagsResponse where
