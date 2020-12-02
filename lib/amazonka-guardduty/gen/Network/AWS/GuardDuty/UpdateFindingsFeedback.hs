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
-- Module      : Network.AWS.GuardDuty.UpdateFindingsFeedback
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks the specified GuardDuty findings as useful or not useful.
module Network.AWS.GuardDuty.UpdateFindingsFeedback
  ( -- * Creating a Request
    updateFindingsFeedback,
    UpdateFindingsFeedback,

    -- * Request Lenses
    uffComments,
    uffDetectorId,
    uffFindingIds,
    uffFeedback,

    -- * Destructuring the Response
    updateFindingsFeedbackResponse,
    UpdateFindingsFeedbackResponse,

    -- * Response Lenses
    uffrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { _uffComments ::
      !(Maybe Text),
    _uffDetectorId :: !Text,
    _uffFindingIds :: ![Text],
    _uffFeedback :: !Feedback
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFindingsFeedback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uffComments' - Additional feedback about the GuardDuty findings.
--
-- * 'uffDetectorId' - The ID of the detector associated with the findings to update feedback for.
--
-- * 'uffFindingIds' - The IDs of the findings that you want to mark as useful or not useful.
--
-- * 'uffFeedback' - The feedback for the finding.
updateFindingsFeedback ::
  -- | 'uffDetectorId'
  Text ->
  -- | 'uffFeedback'
  Feedback ->
  UpdateFindingsFeedback
updateFindingsFeedback pDetectorId_ pFeedback_ =
  UpdateFindingsFeedback'
    { _uffComments = Nothing,
      _uffDetectorId = pDetectorId_,
      _uffFindingIds = mempty,
      _uffFeedback = pFeedback_
    }

-- | Additional feedback about the GuardDuty findings.
uffComments :: Lens' UpdateFindingsFeedback (Maybe Text)
uffComments = lens _uffComments (\s a -> s {_uffComments = a})

-- | The ID of the detector associated with the findings to update feedback for.
uffDetectorId :: Lens' UpdateFindingsFeedback Text
uffDetectorId = lens _uffDetectorId (\s a -> s {_uffDetectorId = a})

-- | The IDs of the findings that you want to mark as useful or not useful.
uffFindingIds :: Lens' UpdateFindingsFeedback [Text]
uffFindingIds = lens _uffFindingIds (\s a -> s {_uffFindingIds = a}) . _Coerce

-- | The feedback for the finding.
uffFeedback :: Lens' UpdateFindingsFeedback Feedback
uffFeedback = lens _uffFeedback (\s a -> s {_uffFeedback = a})

instance AWSRequest UpdateFindingsFeedback where
  type Rs UpdateFindingsFeedback = UpdateFindingsFeedbackResponse
  request = postJSON guardDuty
  response =
    receiveEmpty
      ( \s h x ->
          UpdateFindingsFeedbackResponse' <$> (pure (fromEnum s))
      )

instance Hashable UpdateFindingsFeedback

instance NFData UpdateFindingsFeedback

instance ToHeaders UpdateFindingsFeedback where
  toHeaders =
    const
      ( mconcat
          ["Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)]
      )

instance ToJSON UpdateFindingsFeedback where
  toJSON UpdateFindingsFeedback' {..} =
    object
      ( catMaybes
          [ ("comments" .=) <$> _uffComments,
            Just ("findingIds" .= _uffFindingIds),
            Just ("feedback" .= _uffFeedback)
          ]
      )

instance ToPath UpdateFindingsFeedback where
  toPath UpdateFindingsFeedback' {..} =
    mconcat ["/detector/", toBS _uffDetectorId, "/findings/feedback"]

instance ToQuery UpdateFindingsFeedback where
  toQuery = const mempty

-- | /See:/ 'updateFindingsFeedbackResponse' smart constructor.
newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { _uffrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateFindingsFeedbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uffrsResponseStatus' - -- | The response status code.
updateFindingsFeedbackResponse ::
  -- | 'uffrsResponseStatus'
  Int ->
  UpdateFindingsFeedbackResponse
updateFindingsFeedbackResponse pResponseStatus_ =
  UpdateFindingsFeedbackResponse'
    { _uffrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
uffrsResponseStatus :: Lens' UpdateFindingsFeedbackResponse Int
uffrsResponseStatus = lens _uffrsResponseStatus (\s a -> s {_uffrsResponseStatus = a})

instance NFData UpdateFindingsFeedbackResponse
