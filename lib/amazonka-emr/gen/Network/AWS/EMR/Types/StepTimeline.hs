{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepTimeline
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepTimeline where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The timeline of the cluster step lifecycle.
--
--
--
-- /See:/ 'stepTimeline' smart constructor.
data StepTimeline = StepTimeline'
  { _stCreationDateTime ::
      !(Maybe POSIX),
    _stEndDateTime :: !(Maybe POSIX),
    _stStartDateTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepTimeline' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCreationDateTime' - The date and time when the cluster step was created.
--
-- * 'stEndDateTime' - The date and time when the cluster step execution completed or failed.
--
-- * 'stStartDateTime' - The date and time when the cluster step execution started.
stepTimeline ::
  StepTimeline
stepTimeline =
  StepTimeline'
    { _stCreationDateTime = Nothing,
      _stEndDateTime = Nothing,
      _stStartDateTime = Nothing
    }

-- | The date and time when the cluster step was created.
stCreationDateTime :: Lens' StepTimeline (Maybe UTCTime)
stCreationDateTime = lens _stCreationDateTime (\s a -> s {_stCreationDateTime = a}) . mapping _Time

-- | The date and time when the cluster step execution completed or failed.
stEndDateTime :: Lens' StepTimeline (Maybe UTCTime)
stEndDateTime = lens _stEndDateTime (\s a -> s {_stEndDateTime = a}) . mapping _Time

-- | The date and time when the cluster step execution started.
stStartDateTime :: Lens' StepTimeline (Maybe UTCTime)
stStartDateTime = lens _stStartDateTime (\s a -> s {_stStartDateTime = a}) . mapping _Time

instance FromJSON StepTimeline where
  parseJSON =
    withObject
      "StepTimeline"
      ( \x ->
          StepTimeline'
            <$> (x .:? "CreationDateTime")
            <*> (x .:? "EndDateTime")
            <*> (x .:? "StartDateTime")
      )

instance Hashable StepTimeline

instance NFData StepTimeline
