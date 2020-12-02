{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.TracingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.TracingConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Selects whether or not the state machine's AWS X-Ray tracing is enabled. Default is @false@
--
--
--
-- /See:/ 'tracingConfiguration' smart constructor.
newtype TracingConfiguration = TracingConfiguration'
  { _tcEnabled ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TracingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tcEnabled' - When set to @true@ , AWS X-Ray tracing is enabled.
tracingConfiguration ::
  TracingConfiguration
tracingConfiguration = TracingConfiguration' {_tcEnabled = Nothing}

-- | When set to @true@ , AWS X-Ray tracing is enabled.
tcEnabled :: Lens' TracingConfiguration (Maybe Bool)
tcEnabled = lens _tcEnabled (\s a -> s {_tcEnabled = a})

instance FromJSON TracingConfiguration where
  parseJSON =
    withObject
      "TracingConfiguration"
      (\x -> TracingConfiguration' <$> (x .:? "enabled"))

instance Hashable TracingConfiguration

instance NFData TracingConfiguration

instance ToJSON TracingConfiguration where
  toJSON TracingConfiguration' {..} =
    object (catMaybes [("enabled" .=) <$> _tcEnabled])
