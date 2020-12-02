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
-- Module      : Network.AWS.Inspector.DescribeExclusions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the exclusions that are specified by the exclusions' ARNs.
module Network.AWS.Inspector.DescribeExclusions
  ( -- * Creating a Request
    describeExclusions,
    DescribeExclusions,

    -- * Request Lenses
    deLocale,
    deExclusionARNs,

    -- * Destructuring the Response
    describeExclusionsResponse,
    DescribeExclusionsResponse,

    -- * Response Lenses
    dersResponseStatus,
    dersExclusions,
    dersFailedItems,
  )
where

import Network.AWS.Inspector.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeExclusions' smart constructor.
data DescribeExclusions = DescribeExclusions'
  { _deLocale ::
      !(Maybe Locale),
    _deExclusionARNs :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExclusions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deLocale' - The locale into which you want to translate the exclusion's title, description, and recommendation.
--
-- * 'deExclusionARNs' - The list of ARNs that specify the exclusions that you want to describe.
describeExclusions ::
  -- | 'deExclusionARNs'
  NonEmpty Text ->
  DescribeExclusions
describeExclusions pExclusionARNs_ =
  DescribeExclusions'
    { _deLocale = Nothing,
      _deExclusionARNs = _List1 # pExclusionARNs_
    }

-- | The locale into which you want to translate the exclusion's title, description, and recommendation.
deLocale :: Lens' DescribeExclusions (Maybe Locale)
deLocale = lens _deLocale (\s a -> s {_deLocale = a})

-- | The list of ARNs that specify the exclusions that you want to describe.
deExclusionARNs :: Lens' DescribeExclusions (NonEmpty Text)
deExclusionARNs = lens _deExclusionARNs (\s a -> s {_deExclusionARNs = a}) . _List1

instance AWSRequest DescribeExclusions where
  type Rs DescribeExclusions = DescribeExclusionsResponse
  request = postJSON inspector
  response =
    receiveJSON
      ( \s h x ->
          DescribeExclusionsResponse'
            <$> (pure (fromEnum s))
            <*> (x .?> "exclusions" .!@ mempty)
            <*> (x .?> "failedItems" .!@ mempty)
      )

instance Hashable DescribeExclusions

instance NFData DescribeExclusions

instance ToHeaders DescribeExclusions where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("InspectorService.DescribeExclusions" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeExclusions where
  toJSON DescribeExclusions' {..} =
    object
      ( catMaybes
          [ ("locale" .=) <$> _deLocale,
            Just ("exclusionArns" .= _deExclusionARNs)
          ]
      )

instance ToPath DescribeExclusions where
  toPath = const "/"

instance ToQuery DescribeExclusions where
  toQuery = const mempty

-- | /See:/ 'describeExclusionsResponse' smart constructor.
data DescribeExclusionsResponse = DescribeExclusionsResponse'
  { _dersResponseStatus ::
      !Int,
    _dersExclusions ::
      !(Map Text (Exclusion)),
    _dersFailedItems ::
      !(Map Text (FailedItemDetails))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeExclusionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dersResponseStatus' - -- | The response status code.
--
-- * 'dersExclusions' - Information about the exclusions.
--
-- * 'dersFailedItems' - Exclusion details that cannot be described. An error code is provided for each failed item.
describeExclusionsResponse ::
  -- | 'dersResponseStatus'
  Int ->
  DescribeExclusionsResponse
describeExclusionsResponse pResponseStatus_ =
  DescribeExclusionsResponse'
    { _dersResponseStatus =
        pResponseStatus_,
      _dersExclusions = mempty,
      _dersFailedItems = mempty
    }

-- | -- | The response status code.
dersResponseStatus :: Lens' DescribeExclusionsResponse Int
dersResponseStatus = lens _dersResponseStatus (\s a -> s {_dersResponseStatus = a})

-- | Information about the exclusions.
dersExclusions :: Lens' DescribeExclusionsResponse (HashMap Text (Exclusion))
dersExclusions = lens _dersExclusions (\s a -> s {_dersExclusions = a}) . _Map

-- | Exclusion details that cannot be described. An error code is provided for each failed item.
dersFailedItems :: Lens' DescribeExclusionsResponse (HashMap Text (FailedItemDetails))
dersFailedItems = lens _dersFailedItems (\s a -> s {_dersFailedItems = a}) . _Map

instance NFData DescribeExclusionsResponse
