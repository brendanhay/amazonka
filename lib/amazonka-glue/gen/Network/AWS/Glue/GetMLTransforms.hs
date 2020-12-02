{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a sortable, filterable list of existing AWS Glue machine learning transforms. Machine learning transforms are a special type of transform that use machine learning to learn the details of the transformation to be performed by learning from examples provided by humans. These transformations are then saved by AWS Glue, and you can retrieve their metadata by calling @GetMLTransforms@ .
module Network.AWS.Glue.GetMLTransforms
  ( -- * Creating a Request
    getMLTransforms,
    GetMLTransforms,

    -- * Request Lenses
    gmltNextToken,
    gmltSort,
    gmltFilter,
    gmltMaxResults,

    -- * Destructuring the Response
    getMLTransformsResponse,
    GetMLTransformsResponse,

    -- * Response Lenses
    gmltsrsNextToken,
    gmltsrsResponseStatus,
    gmltsrsTransforms,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getMLTransforms' smart constructor.
data GetMLTransforms = GetMLTransforms'
  { _gmltNextToken ::
      !(Maybe Text),
    _gmltSort :: !(Maybe TransformSortCriteria),
    _gmltFilter :: !(Maybe TransformFilterCriteria),
    _gmltMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTransforms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltNextToken' - A paginated token to offset the results.
--
-- * 'gmltSort' - The sorting criteria.
--
-- * 'gmltFilter' - The filter transformation criteria.
--
-- * 'gmltMaxResults' - The maximum number of results to return.
getMLTransforms ::
  GetMLTransforms
getMLTransforms =
  GetMLTransforms'
    { _gmltNextToken = Nothing,
      _gmltSort = Nothing,
      _gmltFilter = Nothing,
      _gmltMaxResults = Nothing
    }

-- | A paginated token to offset the results.
gmltNextToken :: Lens' GetMLTransforms (Maybe Text)
gmltNextToken = lens _gmltNextToken (\s a -> s {_gmltNextToken = a})

-- | The sorting criteria.
gmltSort :: Lens' GetMLTransforms (Maybe TransformSortCriteria)
gmltSort = lens _gmltSort (\s a -> s {_gmltSort = a})

-- | The filter transformation criteria.
gmltFilter :: Lens' GetMLTransforms (Maybe TransformFilterCriteria)
gmltFilter = lens _gmltFilter (\s a -> s {_gmltFilter = a})

-- | The maximum number of results to return.
gmltMaxResults :: Lens' GetMLTransforms (Maybe Natural)
gmltMaxResults = lens _gmltMaxResults (\s a -> s {_gmltMaxResults = a}) . mapping _Nat

instance AWSRequest GetMLTransforms where
  type Rs GetMLTransforms = GetMLTransformsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          GetMLTransformsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "Transforms" .!@ mempty)
      )

instance Hashable GetMLTransforms

instance NFData GetMLTransforms

instance ToHeaders GetMLTransforms where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.GetMLTransforms" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetMLTransforms where
  toJSON GetMLTransforms' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _gmltNextToken,
            ("Sort" .=) <$> _gmltSort,
            ("Filter" .=) <$> _gmltFilter,
            ("MaxResults" .=) <$> _gmltMaxResults
          ]
      )

instance ToPath GetMLTransforms where
  toPath = const "/"

instance ToQuery GetMLTransforms where
  toQuery = const mempty

-- | /See:/ 'getMLTransformsResponse' smart constructor.
data GetMLTransformsResponse = GetMLTransformsResponse'
  { _gmltsrsNextToken ::
      !(Maybe Text),
    _gmltsrsResponseStatus :: !Int,
    _gmltsrsTransforms :: ![MLTransform]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetMLTransformsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gmltsrsNextToken' - A pagination token, if more results are available.
--
-- * 'gmltsrsResponseStatus' - -- | The response status code.
--
-- * 'gmltsrsTransforms' - A list of machine learning transforms.
getMLTransformsResponse ::
  -- | 'gmltsrsResponseStatus'
  Int ->
  GetMLTransformsResponse
getMLTransformsResponse pResponseStatus_ =
  GetMLTransformsResponse'
    { _gmltsrsNextToken = Nothing,
      _gmltsrsResponseStatus = pResponseStatus_,
      _gmltsrsTransforms = mempty
    }

-- | A pagination token, if more results are available.
gmltsrsNextToken :: Lens' GetMLTransformsResponse (Maybe Text)
gmltsrsNextToken = lens _gmltsrsNextToken (\s a -> s {_gmltsrsNextToken = a})

-- | -- | The response status code.
gmltsrsResponseStatus :: Lens' GetMLTransformsResponse Int
gmltsrsResponseStatus = lens _gmltsrsResponseStatus (\s a -> s {_gmltsrsResponseStatus = a})

-- | A list of machine learning transforms.
gmltsrsTransforms :: Lens' GetMLTransformsResponse [MLTransform]
gmltsrsTransforms = lens _gmltsrsTransforms (\s a -> s {_gmltsrsTransforms = a}) . _Coerce

instance NFData GetMLTransformsResponse
