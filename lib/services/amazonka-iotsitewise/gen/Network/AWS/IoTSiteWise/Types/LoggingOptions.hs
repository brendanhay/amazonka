{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTSiteWise.Types.LoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTSiteWise.Types.LoggingOptions where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTSiteWise.Types.LoggingLevel
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains logging options.
--
-- /See:/ 'newLoggingOptions' smart constructor.
data LoggingOptions = LoggingOptions'
  { -- | The IoT SiteWise logging verbosity level.
    level :: LoggingLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'level', 'loggingOptions_level' - The IoT SiteWise logging verbosity level.
newLoggingOptions ::
  -- | 'level'
  LoggingLevel ->
  LoggingOptions
newLoggingOptions pLevel_ =
  LoggingOptions' {level = pLevel_}

-- | The IoT SiteWise logging verbosity level.
loggingOptions_level :: Lens.Lens' LoggingOptions LoggingLevel
loggingOptions_level = Lens.lens (\LoggingOptions' {level} -> level) (\s@LoggingOptions' {} a -> s {level = a} :: LoggingOptions)

instance Core.FromJSON LoggingOptions where
  parseJSON =
    Core.withObject
      "LoggingOptions"
      ( \x ->
          LoggingOptions' Prelude.<$> (x Core..: "level")
      )

instance Prelude.Hashable LoggingOptions

instance Prelude.NFData LoggingOptions

instance Core.ToJSON LoggingOptions where
  toJSON LoggingOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("level" Core..= level)]
      )
