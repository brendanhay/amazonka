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
-- Module      : Network.AWS.CostExplorer.ProvideAnomalyFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the feedback property of a given cost anomaly.
module Network.AWS.CostExplorer.ProvideAnomalyFeedback
  ( -- * Creating a Request
    provideAnomalyFeedback,
    ProvideAnomalyFeedback,

    -- * Request Lenses
    pafAnomalyId,
    pafFeedback,

    -- * Destructuring the Response
    provideAnomalyFeedbackResponse,
    ProvideAnomalyFeedbackResponse,

    -- * Response Lenses
    pafrsResponseStatus,
    pafrsAnomalyId,
  )
where

import Network.AWS.CostExplorer.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'provideAnomalyFeedback' smart constructor.
data ProvideAnomalyFeedback = ProvideAnomalyFeedback'
  { _pafAnomalyId ::
      !Text,
    _pafFeedback :: !AnomalyFeedbackType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvideAnomalyFeedback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pafAnomalyId' - A cost anomaly ID.
--
-- * 'pafFeedback' - Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
provideAnomalyFeedback ::
  -- | 'pafAnomalyId'
  Text ->
  -- | 'pafFeedback'
  AnomalyFeedbackType ->
  ProvideAnomalyFeedback
provideAnomalyFeedback pAnomalyId_ pFeedback_ =
  ProvideAnomalyFeedback'
    { _pafAnomalyId = pAnomalyId_,
      _pafFeedback = pFeedback_
    }

-- | A cost anomaly ID.
pafAnomalyId :: Lens' ProvideAnomalyFeedback Text
pafAnomalyId = lens _pafAnomalyId (\s a -> s {_pafAnomalyId = a})

-- | Describes whether the cost anomaly was a planned activity or you considered it an anomaly.
pafFeedback :: Lens' ProvideAnomalyFeedback AnomalyFeedbackType
pafFeedback = lens _pafFeedback (\s a -> s {_pafFeedback = a})

instance AWSRequest ProvideAnomalyFeedback where
  type Rs ProvideAnomalyFeedback = ProvideAnomalyFeedbackResponse
  request = postJSON costExplorer
  response =
    receiveJSON
      ( \s h x ->
          ProvideAnomalyFeedbackResponse'
            <$> (pure (fromEnum s)) <*> (x .:> "AnomalyId")
      )

instance Hashable ProvideAnomalyFeedback

instance NFData ProvideAnomalyFeedback

instance ToHeaders ProvideAnomalyFeedback where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AWSInsightsIndexService.ProvideAnomalyFeedback" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ProvideAnomalyFeedback where
  toJSON ProvideAnomalyFeedback' {..} =
    object
      ( catMaybes
          [ Just ("AnomalyId" .= _pafAnomalyId),
            Just ("Feedback" .= _pafFeedback)
          ]
      )

instance ToPath ProvideAnomalyFeedback where
  toPath = const "/"

instance ToQuery ProvideAnomalyFeedback where
  toQuery = const mempty

-- | /See:/ 'provideAnomalyFeedbackResponse' smart constructor.
data ProvideAnomalyFeedbackResponse = ProvideAnomalyFeedbackResponse'
  { _pafrsResponseStatus ::
      !Int,
    _pafrsAnomalyId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProvideAnomalyFeedbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pafrsResponseStatus' - -- | The response status code.
--
-- * 'pafrsAnomalyId' - The ID of the modified cost anomaly.
provideAnomalyFeedbackResponse ::
  -- | 'pafrsResponseStatus'
  Int ->
  -- | 'pafrsAnomalyId'
  Text ->
  ProvideAnomalyFeedbackResponse
provideAnomalyFeedbackResponse pResponseStatus_ pAnomalyId_ =
  ProvideAnomalyFeedbackResponse'
    { _pafrsResponseStatus =
        pResponseStatus_,
      _pafrsAnomalyId = pAnomalyId_
    }

-- | -- | The response status code.
pafrsResponseStatus :: Lens' ProvideAnomalyFeedbackResponse Int
pafrsResponseStatus = lens _pafrsResponseStatus (\s a -> s {_pafrsResponseStatus = a})

-- | The ID of the modified cost anomaly.
pafrsAnomalyId :: Lens' ProvideAnomalyFeedbackResponse Text
pafrsAnomalyId = lens _pafrsAnomalyId (\s a -> s {_pafrsAnomalyId = a})

instance NFData ProvideAnomalyFeedbackResponse
