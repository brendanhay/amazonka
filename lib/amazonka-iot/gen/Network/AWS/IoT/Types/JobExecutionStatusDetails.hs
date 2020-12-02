{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionStatusDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatusDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details of the job execution status.
--
--
--
-- /See:/ 'jobExecutionStatusDetails' smart constructor.
newtype JobExecutionStatusDetails = JobExecutionStatusDetails'
  { _jesdDetailsMap ::
      Maybe (Map Text (Text))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobExecutionStatusDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jesdDetailsMap' - The job execution status.
jobExecutionStatusDetails ::
  JobExecutionStatusDetails
jobExecutionStatusDetails =
  JobExecutionStatusDetails' {_jesdDetailsMap = Nothing}

-- | The job execution status.
jesdDetailsMap :: Lens' JobExecutionStatusDetails (HashMap Text (Text))
jesdDetailsMap = lens _jesdDetailsMap (\s a -> s {_jesdDetailsMap = a}) . _Default . _Map

instance FromJSON JobExecutionStatusDetails where
  parseJSON =
    withObject
      "JobExecutionStatusDetails"
      ( \x ->
          JobExecutionStatusDetails' <$> (x .:? "detailsMap" .!= mempty)
      )

instance Hashable JobExecutionStatusDetails

instance NFData JobExecutionStatusDetails
