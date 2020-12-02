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
-- Module      : Network.AWS.CloudTrail.PutInsightSelectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lets you enable Insights event logging by specifying the Insights selectors that you want to enable on an existing trail. You also use @PutInsightSelectors@ to turn off Insights event logging, by passing an empty list of insight types. In this release, only @ApiCallRateInsight@ is supported as an Insights selector.
module Network.AWS.CloudTrail.PutInsightSelectors
  ( -- * Creating a Request
    putInsightSelectors,
    PutInsightSelectors,

    -- * Request Lenses
    pisTrailName,
    pisInsightSelectors,

    -- * Destructuring the Response
    putInsightSelectorsResponse,
    PutInsightSelectorsResponse,

    -- * Response Lenses
    pisrsTrailARN,
    pisrsInsightSelectors,
    pisrsResponseStatus,
  )
where

import Network.AWS.CloudTrail.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putInsightSelectors' smart constructor.
data PutInsightSelectors = PutInsightSelectors'
  { _pisTrailName ::
      !Text,
    _pisInsightSelectors :: ![InsightSelector]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutInsightSelectors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pisTrailName' - The name of the CloudTrail trail for which you want to change or add Insights selectors.
--
-- * 'pisInsightSelectors' - A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
putInsightSelectors ::
  -- | 'pisTrailName'
  Text ->
  PutInsightSelectors
putInsightSelectors pTrailName_ =
  PutInsightSelectors'
    { _pisTrailName = pTrailName_,
      _pisInsightSelectors = mempty
    }

-- | The name of the CloudTrail trail for which you want to change or add Insights selectors.
pisTrailName :: Lens' PutInsightSelectors Text
pisTrailName = lens _pisTrailName (\s a -> s {_pisTrailName = a})

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
pisInsightSelectors :: Lens' PutInsightSelectors [InsightSelector]
pisInsightSelectors = lens _pisInsightSelectors (\s a -> s {_pisInsightSelectors = a}) . _Coerce

instance AWSRequest PutInsightSelectors where
  type Rs PutInsightSelectors = PutInsightSelectorsResponse
  request = postJSON cloudTrail
  response =
    receiveJSON
      ( \s h x ->
          PutInsightSelectorsResponse'
            <$> (x .?> "TrailARN")
            <*> (x .?> "InsightSelectors" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable PutInsightSelectors

instance NFData PutInsightSelectors

instance ToHeaders PutInsightSelectors where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutInsightSelectors" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutInsightSelectors where
  toJSON PutInsightSelectors' {..} =
    object
      ( catMaybes
          [ Just ("TrailName" .= _pisTrailName),
            Just ("InsightSelectors" .= _pisInsightSelectors)
          ]
      )

instance ToPath PutInsightSelectors where
  toPath = const "/"

instance ToQuery PutInsightSelectors where
  toQuery = const mempty

-- | /See:/ 'putInsightSelectorsResponse' smart constructor.
data PutInsightSelectorsResponse = PutInsightSelectorsResponse'
  { _pisrsTrailARN ::
      !(Maybe Text),
    _pisrsInsightSelectors ::
      !(Maybe [InsightSelector]),
    _pisrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutInsightSelectorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pisrsTrailARN' - The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
--
-- * 'pisrsInsightSelectors' - A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
--
-- * 'pisrsResponseStatus' - -- | The response status code.
putInsightSelectorsResponse ::
  -- | 'pisrsResponseStatus'
  Int ->
  PutInsightSelectorsResponse
putInsightSelectorsResponse pResponseStatus_ =
  PutInsightSelectorsResponse'
    { _pisrsTrailARN = Nothing,
      _pisrsInsightSelectors = Nothing,
      _pisrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of a trail for which you want to change or add Insights selectors.
pisrsTrailARN :: Lens' PutInsightSelectorsResponse (Maybe Text)
pisrsTrailARN = lens _pisrsTrailARN (\s a -> s {_pisrsTrailARN = a})

-- | A JSON string that contains the insight types you want to log on a trail. In this release, only @ApiCallRateInsight@ is supported as an insight type.
pisrsInsightSelectors :: Lens' PutInsightSelectorsResponse [InsightSelector]
pisrsInsightSelectors = lens _pisrsInsightSelectors (\s a -> s {_pisrsInsightSelectors = a}) . _Default . _Coerce

-- | -- | The response status code.
pisrsResponseStatus :: Lens' PutInsightSelectorsResponse Int
pisrsResponseStatus = lens _pisrsResponseStatus (\s a -> s {_pisrsResponseStatus = a})

instance NFData PutInsightSelectorsResponse
