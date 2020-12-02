{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ClusterTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ClusterTimeline where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the timeline of the cluster's lifecycle.
--
--
--
-- /See:/ 'clusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
  { _ctReadyDateTime ::
      !(Maybe POSIX),
    _ctCreationDateTime :: !(Maybe POSIX),
    _ctEndDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ctReadyDateTime' - The date and time when the cluster was ready to run steps.
--
-- * 'ctCreationDateTime' - The creation date and time of the cluster.
--
-- * 'ctEndDateTime' - The date and time when the cluster was terminated.
clusterTimeline ::
  ClusterTimeline
clusterTimeline =
  ClusterTimeline'
    { _ctReadyDateTime = Nothing,
      _ctCreationDateTime = Nothing,
      _ctEndDateTime = Nothing
    }

-- | The date and time when the cluster was ready to run steps.
ctReadyDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctReadyDateTime = lens _ctReadyDateTime (\s a -> s {_ctReadyDateTime = a}) . mapping _Time

-- | The creation date and time of the cluster.
ctCreationDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctCreationDateTime = lens _ctCreationDateTime (\s a -> s {_ctCreationDateTime = a}) . mapping _Time

-- | The date and time when the cluster was terminated.
ctEndDateTime :: Lens' ClusterTimeline (Maybe UTCTime)
ctEndDateTime = lens _ctEndDateTime (\s a -> s {_ctEndDateTime = a}) . mapping _Time

instance FromJSON ClusterTimeline where
  parseJSON =
    withObject
      "ClusterTimeline"
      ( \x ->
          ClusterTimeline'
            <$> (x .:? "ReadyDateTime")
            <*> (x .:? "CreationDateTime")
            <*> (x .:? "EndDateTime")
      )

instance Hashable ClusterTimeline

instance NFData ClusterTimeline
