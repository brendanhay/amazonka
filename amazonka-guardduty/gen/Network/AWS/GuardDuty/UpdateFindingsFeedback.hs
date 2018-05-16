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
-- Module      : Network.AWS.GuardDuty.UpdateFindingsFeedback
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Marks specified Amazon GuardDuty findings as useful or not useful.
module Network.AWS.GuardDuty.UpdateFindingsFeedback
    (
    -- * Creating a Request
      updateFindingsFeedback
    , UpdateFindingsFeedback
    -- * Request Lenses
    , uffFindingIds
    , uffComments
    , uffFeedback
    , uffDetectorId

    -- * Destructuring the Response
    , updateFindingsFeedbackResponse
    , UpdateFindingsFeedbackResponse
    -- * Response Lenses
    , uffrsResponseStatus
    ) where

import Network.AWS.GuardDuty.Types
import Network.AWS.GuardDuty.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | UpdateFindingsFeedback request body.
--
-- /See:/ 'updateFindingsFeedback' smart constructor.
data UpdateFindingsFeedback = UpdateFindingsFeedback'
  { _uffFindingIds :: !(Maybe [Text])
  , _uffComments   :: !(Maybe Text)
  , _uffFeedback   :: !(Maybe Feedback)
  , _uffDetectorId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFindingsFeedback' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uffFindingIds' - IDs of the findings that you want to mark as useful or not useful.
--
-- * 'uffComments' - Additional feedback about the GuardDuty findings.
--
-- * 'uffFeedback' - Valid values: USEFUL | NOT_USEFUL
--
-- * 'uffDetectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to mark as useful or not useful.
updateFindingsFeedback
    :: Text -- ^ 'uffDetectorId'
    -> UpdateFindingsFeedback
updateFindingsFeedback pDetectorId_ =
  UpdateFindingsFeedback'
    { _uffFindingIds = Nothing
    , _uffComments = Nothing
    , _uffFeedback = Nothing
    , _uffDetectorId = pDetectorId_
    }


-- | IDs of the findings that you want to mark as useful or not useful.
uffFindingIds :: Lens' UpdateFindingsFeedback [Text]
uffFindingIds = lens _uffFindingIds (\ s a -> s{_uffFindingIds = a}) . _Default . _Coerce

-- | Additional feedback about the GuardDuty findings.
uffComments :: Lens' UpdateFindingsFeedback (Maybe Text)
uffComments = lens _uffComments (\ s a -> s{_uffComments = a})

-- | Valid values: USEFUL | NOT_USEFUL
uffFeedback :: Lens' UpdateFindingsFeedback (Maybe Feedback)
uffFeedback = lens _uffFeedback (\ s a -> s{_uffFeedback = a})

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to mark as useful or not useful.
uffDetectorId :: Lens' UpdateFindingsFeedback Text
uffDetectorId = lens _uffDetectorId (\ s a -> s{_uffDetectorId = a})

instance AWSRequest UpdateFindingsFeedback where
        type Rs UpdateFindingsFeedback =
             UpdateFindingsFeedbackResponse
        request = postJSON guardDuty
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateFindingsFeedbackResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateFindingsFeedback where

instance NFData UpdateFindingsFeedback where

instance ToHeaders UpdateFindingsFeedback where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateFindingsFeedback where
        toJSON UpdateFindingsFeedback'{..}
          = object
              (catMaybes
                 [("findingIds" .=) <$> _uffFindingIds,
                  ("comments" .=) <$> _uffComments,
                  ("feedback" .=) <$> _uffFeedback])

instance ToPath UpdateFindingsFeedback where
        toPath UpdateFindingsFeedback'{..}
          = mconcat
              ["/detector/", toBS _uffDetectorId,
               "/findings/feedback"]

instance ToQuery UpdateFindingsFeedback where
        toQuery = const mempty

-- | /See:/ 'updateFindingsFeedbackResponse' smart constructor.
newtype UpdateFindingsFeedbackResponse = UpdateFindingsFeedbackResponse'
  { _uffrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateFindingsFeedbackResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uffrsResponseStatus' - -- | The response status code.
updateFindingsFeedbackResponse
    :: Int -- ^ 'uffrsResponseStatus'
    -> UpdateFindingsFeedbackResponse
updateFindingsFeedbackResponse pResponseStatus_ =
  UpdateFindingsFeedbackResponse' {_uffrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
uffrsResponseStatus :: Lens' UpdateFindingsFeedbackResponse Int
uffrsResponseStatus = lens _uffrsResponseStatus (\ s a -> s{_uffrsResponseStatus = a})

instance NFData UpdateFindingsFeedbackResponse where
