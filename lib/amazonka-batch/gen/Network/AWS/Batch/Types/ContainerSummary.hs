{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing summary details of a container within a job.
--
--
--
-- /See:/ 'containerSummary' smart constructor.
data ContainerSummary = ContainerSummary'
  { _csReason ::
      !(Maybe Text),
    _csExitCode :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'csReason' - A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
--
-- * 'csExitCode' - The exit code to return upon completion.
containerSummary ::
  ContainerSummary
containerSummary =
  ContainerSummary' {_csReason = Nothing, _csExitCode = Nothing}

-- | A short (255 max characters) human-readable string to provide additional details about a running or stopped container.
csReason :: Lens' ContainerSummary (Maybe Text)
csReason = lens _csReason (\s a -> s {_csReason = a})

-- | The exit code to return upon completion.
csExitCode :: Lens' ContainerSummary (Maybe Int)
csExitCode = lens _csExitCode (\s a -> s {_csExitCode = a})

instance FromJSON ContainerSummary where
  parseJSON =
    withObject
      "ContainerSummary"
      ( \x ->
          ContainerSummary' <$> (x .:? "reason") <*> (x .:? "exitCode")
      )

instance Hashable ContainerSummary

instance NFData ContainerSummary
