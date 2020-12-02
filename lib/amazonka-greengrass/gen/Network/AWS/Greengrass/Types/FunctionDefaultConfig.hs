{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultConfig where

import Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The default configuration that applies to all Lambda functions in the group. Individual Lambda functions can override these settings.
--
-- /See:/ 'functionDefaultConfig' smart constructor.
newtype FunctionDefaultConfig = FunctionDefaultConfig'
  { _fdcExecution ::
      Maybe FunctionDefaultExecutionConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionDefaultConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdcExecution' - Undocumented member.
functionDefaultConfig ::
  FunctionDefaultConfig
functionDefaultConfig =
  FunctionDefaultConfig' {_fdcExecution = Nothing}

-- | Undocumented member.
fdcExecution :: Lens' FunctionDefaultConfig (Maybe FunctionDefaultExecutionConfig)
fdcExecution = lens _fdcExecution (\s a -> s {_fdcExecution = a})

instance FromJSON FunctionDefaultConfig where
  parseJSON =
    withObject
      "FunctionDefaultConfig"
      (\x -> FunctionDefaultConfig' <$> (x .:? "Execution"))

instance Hashable FunctionDefaultConfig

instance NFData FunctionDefaultConfig

instance ToJSON FunctionDefaultConfig where
  toJSON FunctionDefaultConfig' {..} =
    object (catMaybes [("Execution" .=) <$> _fdcExecution])
