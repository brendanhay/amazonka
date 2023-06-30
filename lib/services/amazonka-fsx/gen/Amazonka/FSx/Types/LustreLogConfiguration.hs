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
-- Module      : Amazonka.FSx.Types.LustreLogConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreLogConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types.LustreAccessAuditLogLevel
import qualified Amazonka.Prelude as Prelude

-- | The configuration for Lustre logging used to write the enabled logging
-- events for your Amazon FSx for Lustre file system or Amazon File Cache
-- resource to Amazon CloudWatch Logs.
--
-- /See:/ 'newLustreLogConfiguration' smart constructor.
data LustreLogConfiguration = LustreLogConfiguration'
  { -- | The Amazon Resource Name (ARN) that specifies the destination of the
    -- logs. The destination can be any Amazon CloudWatch Logs log group ARN.
    -- The destination ARN must be in the same Amazon Web Services partition,
    -- Amazon Web Services Region, and Amazon Web Services account as your
    -- Amazon FSx file system.
    destination :: Prelude.Maybe Prelude.Text,
    -- | The data repository events that are logged by Amazon FSx.
    --
    -- -   @WARN_ONLY@ - only warning events are logged.
    --
    -- -   @ERROR_ONLY@ - only error events are logged.
    --
    -- -   @WARN_ERROR@ - both warning events and error events are logged.
    --
    -- -   @DISABLED@ - logging of data repository events is turned off.
    --
    -- Note that Amazon File Cache uses a default setting of @WARN_ERROR@,
    -- which can\'t be changed.
    level :: LustreAccessAuditLogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LustreLogConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'lustreLogConfiguration_destination' - The Amazon Resource Name (ARN) that specifies the destination of the
-- logs. The destination can be any Amazon CloudWatch Logs log group ARN.
-- The destination ARN must be in the same Amazon Web Services partition,
-- Amazon Web Services Region, and Amazon Web Services account as your
-- Amazon FSx file system.
--
-- 'level', 'lustreLogConfiguration_level' - The data repository events that are logged by Amazon FSx.
--
-- -   @WARN_ONLY@ - only warning events are logged.
--
-- -   @ERROR_ONLY@ - only error events are logged.
--
-- -   @WARN_ERROR@ - both warning events and error events are logged.
--
-- -   @DISABLED@ - logging of data repository events is turned off.
--
-- Note that Amazon File Cache uses a default setting of @WARN_ERROR@,
-- which can\'t be changed.
newLustreLogConfiguration ::
  -- | 'level'
  LustreAccessAuditLogLevel ->
  LustreLogConfiguration
newLustreLogConfiguration pLevel_ =
  LustreLogConfiguration'
    { destination =
        Prelude.Nothing,
      level = pLevel_
    }

-- | The Amazon Resource Name (ARN) that specifies the destination of the
-- logs. The destination can be any Amazon CloudWatch Logs log group ARN.
-- The destination ARN must be in the same Amazon Web Services partition,
-- Amazon Web Services Region, and Amazon Web Services account as your
-- Amazon FSx file system.
lustreLogConfiguration_destination :: Lens.Lens' LustreLogConfiguration (Prelude.Maybe Prelude.Text)
lustreLogConfiguration_destination = Lens.lens (\LustreLogConfiguration' {destination} -> destination) (\s@LustreLogConfiguration' {} a -> s {destination = a} :: LustreLogConfiguration)

-- | The data repository events that are logged by Amazon FSx.
--
-- -   @WARN_ONLY@ - only warning events are logged.
--
-- -   @ERROR_ONLY@ - only error events are logged.
--
-- -   @WARN_ERROR@ - both warning events and error events are logged.
--
-- -   @DISABLED@ - logging of data repository events is turned off.
--
-- Note that Amazon File Cache uses a default setting of @WARN_ERROR@,
-- which can\'t be changed.
lustreLogConfiguration_level :: Lens.Lens' LustreLogConfiguration LustreAccessAuditLogLevel
lustreLogConfiguration_level = Lens.lens (\LustreLogConfiguration' {level} -> level) (\s@LustreLogConfiguration' {} a -> s {level = a} :: LustreLogConfiguration)

instance Data.FromJSON LustreLogConfiguration where
  parseJSON =
    Data.withObject
      "LustreLogConfiguration"
      ( \x ->
          LustreLogConfiguration'
            Prelude.<$> (x Data..:? "Destination")
            Prelude.<*> (x Data..: "Level")
      )

instance Prelude.Hashable LustreLogConfiguration where
  hashWithSalt _salt LustreLogConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` level

instance Prelude.NFData LustreLogConfiguration where
  rnf LustreLogConfiguration' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf level
