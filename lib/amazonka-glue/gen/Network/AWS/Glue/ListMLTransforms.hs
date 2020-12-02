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
-- Module      : Network.AWS.Glue.ListMLTransforms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a sortable, filterable list of existing AWS Glue machine learning transforms in this AWS account, or the resources with the specified tag. This operation takes the optional @Tags@ field, which you can use as a filter of the responses so that tagged resources can be retrieved as a group. If you choose to use tag filtering, only resources with the tags are retrieved.
module Network.AWS.Glue.ListMLTransforms
  ( -- * Creating a Request
    listMLTransforms,
    ListMLTransforms,

    -- * Request Lenses
    lmltNextToken,
    lmltSort,
    lmltFilter,
    lmltMaxResults,
    lmltTags,

    -- * Destructuring the Response
    listMLTransformsResponse,
    ListMLTransformsResponse,

    -- * Response Lenses
    lmltrsNextToken,
    lmltrsResponseStatus,
    lmltrsTransformIds,
  )
where

import Network.AWS.Glue.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listMLTransforms' smart constructor.
data ListMLTransforms = ListMLTransforms'
  { _lmltNextToken ::
      !(Maybe Text),
    _lmltSort :: !(Maybe TransformSortCriteria),
    _lmltFilter :: !(Maybe TransformFilterCriteria),
    _lmltMaxResults :: !(Maybe Nat),
    _lmltTags :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMLTransforms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmltNextToken' - A continuation token, if this is a continuation request.
--
-- * 'lmltSort' - A @TransformSortCriteria@ used to sort the machine learning transforms.
--
-- * 'lmltFilter' - A @TransformFilterCriteria@ used to filter the machine learning transforms.
--
-- * 'lmltMaxResults' - The maximum size of a list to return.
--
-- * 'lmltTags' - Specifies to return only these tagged resources.
listMLTransforms ::
  ListMLTransforms
listMLTransforms =
  ListMLTransforms'
    { _lmltNextToken = Nothing,
      _lmltSort = Nothing,
      _lmltFilter = Nothing,
      _lmltMaxResults = Nothing,
      _lmltTags = Nothing
    }

-- | A continuation token, if this is a continuation request.
lmltNextToken :: Lens' ListMLTransforms (Maybe Text)
lmltNextToken = lens _lmltNextToken (\s a -> s {_lmltNextToken = a})

-- | A @TransformSortCriteria@ used to sort the machine learning transforms.
lmltSort :: Lens' ListMLTransforms (Maybe TransformSortCriteria)
lmltSort = lens _lmltSort (\s a -> s {_lmltSort = a})

-- | A @TransformFilterCriteria@ used to filter the machine learning transforms.
lmltFilter :: Lens' ListMLTransforms (Maybe TransformFilterCriteria)
lmltFilter = lens _lmltFilter (\s a -> s {_lmltFilter = a})

-- | The maximum size of a list to return.
lmltMaxResults :: Lens' ListMLTransforms (Maybe Natural)
lmltMaxResults = lens _lmltMaxResults (\s a -> s {_lmltMaxResults = a}) . mapping _Nat

-- | Specifies to return only these tagged resources.
lmltTags :: Lens' ListMLTransforms (HashMap Text (Text))
lmltTags = lens _lmltTags (\s a -> s {_lmltTags = a}) . _Default . _Map

instance AWSRequest ListMLTransforms where
  type Rs ListMLTransforms = ListMLTransformsResponse
  request = postJSON glue
  response =
    receiveJSON
      ( \s h x ->
          ListMLTransformsResponse'
            <$> (x .?> "NextToken")
            <*> (pure (fromEnum s))
            <*> (x .?> "TransformIds" .!@ mempty)
      )

instance Hashable ListMLTransforms

instance NFData ListMLTransforms

instance ToHeaders ListMLTransforms where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target" =# ("AWSGlue.ListMLTransforms" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ListMLTransforms where
  toJSON ListMLTransforms' {..} =
    object
      ( catMaybes
          [ ("NextToken" .=) <$> _lmltNextToken,
            ("Sort" .=) <$> _lmltSort,
            ("Filter" .=) <$> _lmltFilter,
            ("MaxResults" .=) <$> _lmltMaxResults,
            ("Tags" .=) <$> _lmltTags
          ]
      )

instance ToPath ListMLTransforms where
  toPath = const "/"

instance ToQuery ListMLTransforms where
  toQuery = const mempty

-- | /See:/ 'listMLTransformsResponse' smart constructor.
data ListMLTransformsResponse = ListMLTransformsResponse'
  { _lmltrsNextToken ::
      !(Maybe Text),
    _lmltrsResponseStatus :: !Int,
    _lmltrsTransformIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ListMLTransformsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lmltrsNextToken' - A continuation token, if the returned list does not contain the last metric available.
--
-- * 'lmltrsResponseStatus' - -- | The response status code.
--
-- * 'lmltrsTransformIds' - The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
listMLTransformsResponse ::
  -- | 'lmltrsResponseStatus'
  Int ->
  ListMLTransformsResponse
listMLTransformsResponse pResponseStatus_ =
  ListMLTransformsResponse'
    { _lmltrsNextToken = Nothing,
      _lmltrsResponseStatus = pResponseStatus_,
      _lmltrsTransformIds = mempty
    }

-- | A continuation token, if the returned list does not contain the last metric available.
lmltrsNextToken :: Lens' ListMLTransformsResponse (Maybe Text)
lmltrsNextToken = lens _lmltrsNextToken (\s a -> s {_lmltrsNextToken = a})

-- | -- | The response status code.
lmltrsResponseStatus :: Lens' ListMLTransformsResponse Int
lmltrsResponseStatus = lens _lmltrsResponseStatus (\s a -> s {_lmltrsResponseStatus = a})

-- | The identifiers of all the machine learning transforms in the account, or the machine learning transforms with the specified tags.
lmltrsTransformIds :: Lens' ListMLTransformsResponse [Text]
lmltrsTransformIds = lens _lmltrsTransformIds (\s a -> s {_lmltrsTransformIds = a}) . _Coerce

instance NFData ListMLTransformsResponse
