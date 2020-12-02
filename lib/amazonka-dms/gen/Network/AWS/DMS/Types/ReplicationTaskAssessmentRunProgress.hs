{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ReplicationTaskAssessmentRunProgress where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The progress values reported by the @AssessmentProgress@ response element.
--
--
--
-- /See:/ 'replicationTaskAssessmentRunProgress' smart constructor.
data ReplicationTaskAssessmentRunProgress = ReplicationTaskAssessmentRunProgress'
  { _rtarpIndividualAssessmentCount ::
      !(Maybe Int),
    _rtarpIndividualAssessmentCompletedCount ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationTaskAssessmentRunProgress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rtarpIndividualAssessmentCount' - The number of individual assessments that are specified to run.
--
-- * 'rtarpIndividualAssessmentCompletedCount' - The number of individual assessments that have completed, successfully or not.
replicationTaskAssessmentRunProgress ::
  ReplicationTaskAssessmentRunProgress
replicationTaskAssessmentRunProgress =
  ReplicationTaskAssessmentRunProgress'
    { _rtarpIndividualAssessmentCount =
        Nothing,
      _rtarpIndividualAssessmentCompletedCount = Nothing
    }

-- | The number of individual assessments that are specified to run.
rtarpIndividualAssessmentCount :: Lens' ReplicationTaskAssessmentRunProgress (Maybe Int)
rtarpIndividualAssessmentCount = lens _rtarpIndividualAssessmentCount (\s a -> s {_rtarpIndividualAssessmentCount = a})

-- | The number of individual assessments that have completed, successfully or not.
rtarpIndividualAssessmentCompletedCount :: Lens' ReplicationTaskAssessmentRunProgress (Maybe Int)
rtarpIndividualAssessmentCompletedCount = lens _rtarpIndividualAssessmentCompletedCount (\s a -> s {_rtarpIndividualAssessmentCompletedCount = a})

instance FromJSON ReplicationTaskAssessmentRunProgress where
  parseJSON =
    withObject
      "ReplicationTaskAssessmentRunProgress"
      ( \x ->
          ReplicationTaskAssessmentRunProgress'
            <$> (x .:? "IndividualAssessmentCount")
            <*> (x .:? "IndividualAssessmentCompletedCount")
      )

instance Hashable ReplicationTaskAssessmentRunProgress

instance NFData ReplicationTaskAssessmentRunProgress
