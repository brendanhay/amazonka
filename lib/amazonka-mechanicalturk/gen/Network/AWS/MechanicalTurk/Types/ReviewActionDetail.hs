{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.ReviewActionDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.ReviewActionDetail where

import Network.AWS.Lens
import Network.AWS.MechanicalTurk.Types.ReviewActionStatus
import Network.AWS.Prelude

-- | Both the AssignmentReviewReport and the HITReviewReport elements contains the ReviewActionDetail data structure. This structure is returned multiple times for each action specified in the Review Policy.
--
--
--
-- /See:/ 'reviewActionDetail' smart constructor.
data ReviewActionDetail = ReviewActionDetail'
  { _radStatus ::
      !(Maybe ReviewActionStatus),
    _radTargetId :: !(Maybe Text),
    _radActionId :: !(Maybe Text),
    _radTargetType :: !(Maybe Text),
    _radResult :: !(Maybe Text),
    _radActionName :: !(Maybe Text),
    _radCompleteTime :: !(Maybe POSIX),
    _radErrorCode :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReviewActionDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'radStatus' - The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
--
-- * 'radTargetId' - The specific HITId or AssignmentID targeted by the action.
--
-- * 'radActionId' - The unique identifier for the action.
--
-- * 'radTargetType' - The type of object in TargetId.
--
-- * 'radResult' - A description of the outcome of the review.
--
-- * 'radActionName' - The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
--
-- * 'radCompleteTime' - The date when the action was completed.
--
-- * 'radErrorCode' - Present only when the Results have a FAILED Status.
reviewActionDetail ::
  ReviewActionDetail
reviewActionDetail =
  ReviewActionDetail'
    { _radStatus = Nothing,
      _radTargetId = Nothing,
      _radActionId = Nothing,
      _radTargetType = Nothing,
      _radResult = Nothing,
      _radActionName = Nothing,
      _radCompleteTime = Nothing,
      _radErrorCode = Nothing
    }

-- | The current disposition of the action: INTENDED, SUCCEEDED, FAILED, or CANCELLED.
radStatus :: Lens' ReviewActionDetail (Maybe ReviewActionStatus)
radStatus = lens _radStatus (\s a -> s {_radStatus = a})

-- | The specific HITId or AssignmentID targeted by the action.
radTargetId :: Lens' ReviewActionDetail (Maybe Text)
radTargetId = lens _radTargetId (\s a -> s {_radTargetId = a})

-- | The unique identifier for the action.
radActionId :: Lens' ReviewActionDetail (Maybe Text)
radActionId = lens _radActionId (\s a -> s {_radActionId = a})

-- | The type of object in TargetId.
radTargetType :: Lens' ReviewActionDetail (Maybe Text)
radTargetType = lens _radTargetType (\s a -> s {_radTargetType = a})

-- | A description of the outcome of the review.
radResult :: Lens' ReviewActionDetail (Maybe Text)
radResult = lens _radResult (\s a -> s {_radResult = a})

-- | The nature of the action itself. The Review Policy is responsible for examining the HIT and Assignments, emitting results, and deciding which other actions will be necessary.
radActionName :: Lens' ReviewActionDetail (Maybe Text)
radActionName = lens _radActionName (\s a -> s {_radActionName = a})

-- | The date when the action was completed.
radCompleteTime :: Lens' ReviewActionDetail (Maybe UTCTime)
radCompleteTime = lens _radCompleteTime (\s a -> s {_radCompleteTime = a}) . mapping _Time

-- | Present only when the Results have a FAILED Status.
radErrorCode :: Lens' ReviewActionDetail (Maybe Text)
radErrorCode = lens _radErrorCode (\s a -> s {_radErrorCode = a})

instance FromJSON ReviewActionDetail where
  parseJSON =
    withObject
      "ReviewActionDetail"
      ( \x ->
          ReviewActionDetail'
            <$> (x .:? "Status")
            <*> (x .:? "TargetId")
            <*> (x .:? "ActionId")
            <*> (x .:? "TargetType")
            <*> (x .:? "Result")
            <*> (x .:? "ActionName")
            <*> (x .:? "CompleteTime")
            <*> (x .:? "ErrorCode")
      )

instance Hashable ReviewActionDetail

instance NFData ReviewActionDetail
