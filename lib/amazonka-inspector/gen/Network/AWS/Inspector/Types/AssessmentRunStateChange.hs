{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.AssessmentRunStateChange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.AssessmentRunStateChange where

import Network.AWS.Inspector.Types.AssessmentRunState
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Used as one of the elements of the 'AssessmentRun' data type.
--
--
--
-- /See:/ 'assessmentRunStateChange' smart constructor.
data AssessmentRunStateChange = AssessmentRunStateChange'
  { _arscStateChangedAt ::
      !POSIX,
    _arscState :: !AssessmentRunState
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AssessmentRunStateChange' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'arscStateChangedAt' - The last time the assessment run state changed.
--
-- * 'arscState' - The assessment run state.
assessmentRunStateChange ::
  -- | 'arscStateChangedAt'
  UTCTime ->
  -- | 'arscState'
  AssessmentRunState ->
  AssessmentRunStateChange
assessmentRunStateChange pStateChangedAt_ pState_ =
  AssessmentRunStateChange'
    { _arscStateChangedAt =
        _Time # pStateChangedAt_,
      _arscState = pState_
    }

-- | The last time the assessment run state changed.
arscStateChangedAt :: Lens' AssessmentRunStateChange UTCTime
arscStateChangedAt = lens _arscStateChangedAt (\s a -> s {_arscStateChangedAt = a}) . _Time

-- | The assessment run state.
arscState :: Lens' AssessmentRunStateChange AssessmentRunState
arscState = lens _arscState (\s a -> s {_arscState = a})

instance FromJSON AssessmentRunStateChange where
  parseJSON =
    withObject
      "AssessmentRunStateChange"
      ( \x ->
          AssessmentRunStateChange'
            <$> (x .: "stateChangedAt") <*> (x .: "state")
      )

instance Hashable AssessmentRunStateChange

instance NFData AssessmentRunStateChange
