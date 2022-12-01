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
-- Module      : Amazonka.FSx.Types.LustreLogCreateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FSx.Types.LustreLogCreateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.FSx.Types.LustreAccessAuditLogLevel
import qualified Amazonka.Prelude as Prelude

-- | The Lustre logging configuration used when creating or updating an
-- Amazon FSx for Lustre file system. An Amazon File Cache is created with
-- Lustre logging enabled by default, with a setting of @WARN_ERROR@ for
-- the logging events. which can\'t be changed.
--
-- Lustre logging writes the enabled logging events for your file system or
-- cache to Amazon CloudWatch Logs.
--
-- /See:/ 'newLustreLogCreateConfiguration' smart constructor.
data LustreLogCreateConfiguration = LustreLogCreateConfiguration'
  { -- | The Amazon Resource Name (ARN) that specifies the destination of the
    -- logs.
    --
    -- The destination can be any Amazon CloudWatch Logs log group ARN, with
    -- the following requirements:
    --
    -- -   The destination ARN that you provide must be in the same Amazon Web
    --     Services partition, Amazon Web Services Region, and Amazon Web
    --     Services account as your Amazon FSx file system.
    --
    -- -   The name of the Amazon CloudWatch Logs log group must begin with the
    --     @\/aws\/fsx@ prefix.
    --
    -- -   If you do not provide a destination, Amazon FSx will create and use
    --     a log stream in the CloudWatch Logs @\/aws\/fsx\/lustre@ log group
    --     (for Amazon FSx for Lustre) or @\/aws\/fsx\/filecache@ (for Amazon
    --     File Cache).
    --
    -- -   If @Destination@ is provided and the resource does not exist, the
    --     request will fail with a @BadRequest@ error.
    --
    -- -   If @Level@ is set to @DISABLED@, you cannot specify a destination in
    --     @Destination@.
    destination :: Prelude.Maybe Prelude.Text,
    -- | Sets which data repository events are logged by Amazon FSx.
    --
    -- -   @WARN_ONLY@ - only warning events are logged.
    --
    -- -   @ERROR_ONLY@ - only error events are logged.
    --
    -- -   @WARN_ERROR@ - both warning events and error events are logged.
    --
    -- -   @DISABLED@ - logging of data repository events is turned off.
    level :: LustreAccessAuditLogLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LustreLogCreateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destination', 'lustreLogCreateConfiguration_destination' - The Amazon Resource Name (ARN) that specifies the destination of the
-- logs.
--
-- The destination can be any Amazon CloudWatch Logs log group ARN, with
-- the following requirements:
--
-- -   The destination ARN that you provide must be in the same Amazon Web
--     Services partition, Amazon Web Services Region, and Amazon Web
--     Services account as your Amazon FSx file system.
--
-- -   The name of the Amazon CloudWatch Logs log group must begin with the
--     @\/aws\/fsx@ prefix.
--
-- -   If you do not provide a destination, Amazon FSx will create and use
--     a log stream in the CloudWatch Logs @\/aws\/fsx\/lustre@ log group
--     (for Amazon FSx for Lustre) or @\/aws\/fsx\/filecache@ (for Amazon
--     File Cache).
--
-- -   If @Destination@ is provided and the resource does not exist, the
--     request will fail with a @BadRequest@ error.
--
-- -   If @Level@ is set to @DISABLED@, you cannot specify a destination in
--     @Destination@.
--
-- 'level', 'lustreLogCreateConfiguration_level' - Sets which data repository events are logged by Amazon FSx.
--
-- -   @WARN_ONLY@ - only warning events are logged.
--
-- -   @ERROR_ONLY@ - only error events are logged.
--
-- -   @WARN_ERROR@ - both warning events and error events are logged.
--
-- -   @DISABLED@ - logging of data repository events is turned off.
newLustreLogCreateConfiguration ::
  -- | 'level'
  LustreAccessAuditLogLevel ->
  LustreLogCreateConfiguration
newLustreLogCreateConfiguration pLevel_ =
  LustreLogCreateConfiguration'
    { destination =
        Prelude.Nothing,
      level = pLevel_
    }

-- | The Amazon Resource Name (ARN) that specifies the destination of the
-- logs.
--
-- The destination can be any Amazon CloudWatch Logs log group ARN, with
-- the following requirements:
--
-- -   The destination ARN that you provide must be in the same Amazon Web
--     Services partition, Amazon Web Services Region, and Amazon Web
--     Services account as your Amazon FSx file system.
--
-- -   The name of the Amazon CloudWatch Logs log group must begin with the
--     @\/aws\/fsx@ prefix.
--
-- -   If you do not provide a destination, Amazon FSx will create and use
--     a log stream in the CloudWatch Logs @\/aws\/fsx\/lustre@ log group
--     (for Amazon FSx for Lustre) or @\/aws\/fsx\/filecache@ (for Amazon
--     File Cache).
--
-- -   If @Destination@ is provided and the resource does not exist, the
--     request will fail with a @BadRequest@ error.
--
-- -   If @Level@ is set to @DISABLED@, you cannot specify a destination in
--     @Destination@.
lustreLogCreateConfiguration_destination :: Lens.Lens' LustreLogCreateConfiguration (Prelude.Maybe Prelude.Text)
lustreLogCreateConfiguration_destination = Lens.lens (\LustreLogCreateConfiguration' {destination} -> destination) (\s@LustreLogCreateConfiguration' {} a -> s {destination = a} :: LustreLogCreateConfiguration)

-- | Sets which data repository events are logged by Amazon FSx.
--
-- -   @WARN_ONLY@ - only warning events are logged.
--
-- -   @ERROR_ONLY@ - only error events are logged.
--
-- -   @WARN_ERROR@ - both warning events and error events are logged.
--
-- -   @DISABLED@ - logging of data repository events is turned off.
lustreLogCreateConfiguration_level :: Lens.Lens' LustreLogCreateConfiguration LustreAccessAuditLogLevel
lustreLogCreateConfiguration_level = Lens.lens (\LustreLogCreateConfiguration' {level} -> level) (\s@LustreLogCreateConfiguration' {} a -> s {level = a} :: LustreLogCreateConfiguration)

instance
  Prelude.Hashable
    LustreLogCreateConfiguration
  where
  hashWithSalt _salt LustreLogCreateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` level

instance Prelude.NFData LustreLogCreateConfiguration where
  rnf LustreLogCreateConfiguration' {..} =
    Prelude.rnf destination
      `Prelude.seq` Prelude.rnf level

instance Core.ToJSON LustreLogCreateConfiguration where
  toJSON LustreLogCreateConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Destination" Core..=) Prelude.<$> destination,
            Prelude.Just ("Level" Core..= level)
          ]
      )
