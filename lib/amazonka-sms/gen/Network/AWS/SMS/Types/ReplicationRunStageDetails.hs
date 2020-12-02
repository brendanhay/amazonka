{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunStageDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunStageDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the current stage of a replication run.
--
--
--
-- /See:/ 'replicationRunStageDetails' smart constructor.
data ReplicationRunStageDetails = ReplicationRunStageDetails'
  { _rrsdStage ::
      !(Maybe Text),
    _rrsdStageProgress :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReplicationRunStageDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrsdStage' - The current stage of a replication run.
--
-- * 'rrsdStageProgress' - The progress of the current stage of a replication run.
replicationRunStageDetails ::
  ReplicationRunStageDetails
replicationRunStageDetails =
  ReplicationRunStageDetails'
    { _rrsdStage = Nothing,
      _rrsdStageProgress = Nothing
    }

-- | The current stage of a replication run.
rrsdStage :: Lens' ReplicationRunStageDetails (Maybe Text)
rrsdStage = lens _rrsdStage (\s a -> s {_rrsdStage = a})

-- | The progress of the current stage of a replication run.
rrsdStageProgress :: Lens' ReplicationRunStageDetails (Maybe Text)
rrsdStageProgress = lens _rrsdStageProgress (\s a -> s {_rrsdStageProgress = a})

instance FromJSON ReplicationRunStageDetails where
  parseJSON =
    withObject
      "ReplicationRunStageDetails"
      ( \x ->
          ReplicationRunStageDetails'
            <$> (x .:? "stage") <*> (x .:? "stageProgress")
      )

instance Hashable ReplicationRunStageDetails

instance NFData ReplicationRunStageDetails
