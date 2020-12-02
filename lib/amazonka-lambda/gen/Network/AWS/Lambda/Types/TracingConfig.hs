{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.TracingConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.TracingConfig where

import Network.AWS.Lambda.Types.TracingMode
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The function's AWS X-Ray tracing configuration. To sample and record incoming requests, set @Mode@ to @Active@ .
--
--
--
-- /See:/ 'tracingConfig' smart constructor.
newtype TracingConfig = TracingConfig' {_tMode :: Maybe TracingMode}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TracingConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tMode' - The tracing mode.
tracingConfig ::
  TracingConfig
tracingConfig = TracingConfig' {_tMode = Nothing}

-- | The tracing mode.
tMode :: Lens' TracingConfig (Maybe TracingMode)
tMode = lens _tMode (\s a -> s {_tMode = a})

instance Hashable TracingConfig

instance NFData TracingConfig

instance ToJSON TracingConfig where
  toJSON TracingConfig' {..} =
    object (catMaybes [("Mode" .=) <$> _tMode])
