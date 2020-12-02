{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionDefaultExecutionConfig where

import Network.AWS.Greengrass.Types.FunctionIsolationMode
import Network.AWS.Greengrass.Types.FunctionRunAsConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration information that specifies how a Lambda function runs.
--
-- /See:/ 'functionDefaultExecutionConfig' smart constructor.
data FunctionDefaultExecutionConfig = FunctionDefaultExecutionConfig'
  { _fdecRunAs ::
      !(Maybe FunctionRunAsConfig),
    _fdecIsolationMode ::
      !( Maybe
           FunctionIsolationMode
       )
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionDefaultExecutionConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdecRunAs' - Undocumented member.
--
-- * 'fdecIsolationMode' - Undocumented member.
functionDefaultExecutionConfig ::
  FunctionDefaultExecutionConfig
functionDefaultExecutionConfig =
  FunctionDefaultExecutionConfig'
    { _fdecRunAs = Nothing,
      _fdecIsolationMode = Nothing
    }

-- | Undocumented member.
fdecRunAs :: Lens' FunctionDefaultExecutionConfig (Maybe FunctionRunAsConfig)
fdecRunAs = lens _fdecRunAs (\s a -> s {_fdecRunAs = a})

-- | Undocumented member.
fdecIsolationMode :: Lens' FunctionDefaultExecutionConfig (Maybe FunctionIsolationMode)
fdecIsolationMode = lens _fdecIsolationMode (\s a -> s {_fdecIsolationMode = a})

instance FromJSON FunctionDefaultExecutionConfig where
  parseJSON =
    withObject
      "FunctionDefaultExecutionConfig"
      ( \x ->
          FunctionDefaultExecutionConfig'
            <$> (x .:? "RunAs") <*> (x .:? "IsolationMode")
      )

instance Hashable FunctionDefaultExecutionConfig

instance NFData FunctionDefaultExecutionConfig

instance ToJSON FunctionDefaultExecutionConfig where
  toJSON FunctionDefaultExecutionConfig' {..} =
    object
      ( catMaybes
          [ ("RunAs" .=) <$> _fdecRunAs,
            ("IsolationMode" .=) <$> _fdecIsolationMode
          ]
      )
