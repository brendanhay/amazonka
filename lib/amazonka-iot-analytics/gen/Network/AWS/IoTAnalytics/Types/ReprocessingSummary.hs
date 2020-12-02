{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingSummary where

import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about pipeline reprocessing.
--
--
--
-- /See:/ 'reprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { _rsCreationTime ::
      !(Maybe POSIX),
    _rsStatus :: !(Maybe ReprocessingStatus),
    _rsId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReprocessingSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsCreationTime' - The time the pipeline reprocessing was created.
--
-- * 'rsStatus' - The status of the pipeline reprocessing.
--
-- * 'rsId' - The @reprocessingId@ returned by @StartPipelineReprocessing@ .
reprocessingSummary ::
  ReprocessingSummary
reprocessingSummary =
  ReprocessingSummary'
    { _rsCreationTime = Nothing,
      _rsStatus = Nothing,
      _rsId = Nothing
    }

-- | The time the pipeline reprocessing was created.
rsCreationTime :: Lens' ReprocessingSummary (Maybe UTCTime)
rsCreationTime = lens _rsCreationTime (\s a -> s {_rsCreationTime = a}) . mapping _Time

-- | The status of the pipeline reprocessing.
rsStatus :: Lens' ReprocessingSummary (Maybe ReprocessingStatus)
rsStatus = lens _rsStatus (\s a -> s {_rsStatus = a})

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@ .
rsId :: Lens' ReprocessingSummary (Maybe Text)
rsId = lens _rsId (\s a -> s {_rsId = a})

instance FromJSON ReprocessingSummary where
  parseJSON =
    withObject
      "ReprocessingSummary"
      ( \x ->
          ReprocessingSummary'
            <$> (x .:? "creationTime") <*> (x .:? "status") <*> (x .:? "id")
      )

instance Hashable ReprocessingSummary

instance NFData ReprocessingSummary
