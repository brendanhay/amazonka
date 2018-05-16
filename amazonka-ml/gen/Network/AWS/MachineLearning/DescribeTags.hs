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
-- Module      : Network.AWS.MachineLearning.DescribeTags
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of the tags for your Amazon ML object.
--
--
module Network.AWS.MachineLearning.DescribeTags
    (
    -- * Creating a Request
      describeTags
    , DescribeTags
    -- * Request Lenses
    , dtResourceId
    , dtResourceType

    -- * Destructuring the Response
    , describeTagsResponse
    , DescribeTagsResponse
    -- * Response Lenses
    , dtrsResourceId
    , dtrsResourceType
    , dtrsTags
    , dtrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.MachineLearning.Types
import Network.AWS.MachineLearning.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTags' smart constructor.
data DescribeTags = DescribeTags'
  { _dtResourceId   :: !Text
  , _dtResourceType :: !TaggableResourceType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTags' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtResourceId' - The ID of the ML object. For example, @exampleModelId@ .
--
-- * 'dtResourceType' - The type of the ML object.
describeTags
    :: Text -- ^ 'dtResourceId'
    -> TaggableResourceType -- ^ 'dtResourceType'
    -> DescribeTags
describeTags pResourceId_ pResourceType_ =
  DescribeTags' {_dtResourceId = pResourceId_, _dtResourceType = pResourceType_}


-- | The ID of the ML object. For example, @exampleModelId@ .
dtResourceId :: Lens' DescribeTags Text
dtResourceId = lens _dtResourceId (\ s a -> s{_dtResourceId = a})

-- | The type of the ML object.
dtResourceType :: Lens' DescribeTags TaggableResourceType
dtResourceType = lens _dtResourceType (\ s a -> s{_dtResourceType = a})

instance AWSRequest DescribeTags where
        type Rs DescribeTags = DescribeTagsResponse
        request = postJSON machineLearning
        response
          = receiveJSON
              (\ s h x ->
                 DescribeTagsResponse' <$>
                   (x .?> "ResourceId") <*> (x .?> "ResourceType") <*>
                     (x .?> "Tags" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeTags where

instance NFData DescribeTags where

instance ToHeaders DescribeTags where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonML_20141212.DescribeTags" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeTags where
        toJSON DescribeTags'{..}
          = object
              (catMaybes
                 [Just ("ResourceId" .= _dtResourceId),
                  Just ("ResourceType" .= _dtResourceType)])

instance ToPath DescribeTags where
        toPath = const "/"

instance ToQuery DescribeTags where
        toQuery = const mempty

-- | Amazon ML returns the following elements.
--
--
--
-- /See:/ 'describeTagsResponse' smart constructor.
data DescribeTagsResponse = DescribeTagsResponse'
  { _dtrsResourceId     :: !(Maybe Text)
  , _dtrsResourceType   :: !(Maybe TaggableResourceType)
  , _dtrsTags           :: !(Maybe [Tag])
  , _dtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeTagsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtrsResourceId' - The ID of the tagged ML object.
--
-- * 'dtrsResourceType' - The type of the tagged ML object.
--
-- * 'dtrsTags' - A list of tags associated with the ML object.
--
-- * 'dtrsResponseStatus' - -- | The response status code.
describeTagsResponse
    :: Int -- ^ 'dtrsResponseStatus'
    -> DescribeTagsResponse
describeTagsResponse pResponseStatus_ =
  DescribeTagsResponse'
    { _dtrsResourceId = Nothing
    , _dtrsResourceType = Nothing
    , _dtrsTags = Nothing
    , _dtrsResponseStatus = pResponseStatus_
    }


-- | The ID of the tagged ML object.
dtrsResourceId :: Lens' DescribeTagsResponse (Maybe Text)
dtrsResourceId = lens _dtrsResourceId (\ s a -> s{_dtrsResourceId = a})

-- | The type of the tagged ML object.
dtrsResourceType :: Lens' DescribeTagsResponse (Maybe TaggableResourceType)
dtrsResourceType = lens _dtrsResourceType (\ s a -> s{_dtrsResourceType = a})

-- | A list of tags associated with the ML object.
dtrsTags :: Lens' DescribeTagsResponse [Tag]
dtrsTags = lens _dtrsTags (\ s a -> s{_dtrsTags = a}) . _Default . _Coerce

-- | -- | The response status code.
dtrsResponseStatus :: Lens' DescribeTagsResponse Int
dtrsResponseStatus = lens _dtrsResponseStatus (\ s a -> s{_dtrsResponseStatus = a})

instance NFData DescribeTagsResponse where
