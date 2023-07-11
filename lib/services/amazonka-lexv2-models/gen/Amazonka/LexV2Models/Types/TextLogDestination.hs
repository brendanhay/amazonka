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
-- Module      : Amazonka.LexV2Models.Types.TextLogDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.TextLogDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.CloudWatchLogGroupLogDestination
import qualified Amazonka.Prelude as Prelude

-- | Defines the Amazon CloudWatch Logs destination log group for
-- conversation text logs.
--
-- /See:/ 'newTextLogDestination' smart constructor.
data TextLogDestination = TextLogDestination'
  { -- | Defines the Amazon CloudWatch Logs log group where text and metadata
    -- logs are delivered.
    cloudWatch :: CloudWatchLogGroupLogDestination
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextLogDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatch', 'textLogDestination_cloudWatch' - Defines the Amazon CloudWatch Logs log group where text and metadata
-- logs are delivered.
newTextLogDestination ::
  -- | 'cloudWatch'
  CloudWatchLogGroupLogDestination ->
  TextLogDestination
newTextLogDestination pCloudWatch_ =
  TextLogDestination' {cloudWatch = pCloudWatch_}

-- | Defines the Amazon CloudWatch Logs log group where text and metadata
-- logs are delivered.
textLogDestination_cloudWatch :: Lens.Lens' TextLogDestination CloudWatchLogGroupLogDestination
textLogDestination_cloudWatch = Lens.lens (\TextLogDestination' {cloudWatch} -> cloudWatch) (\s@TextLogDestination' {} a -> s {cloudWatch = a} :: TextLogDestination)

instance Data.FromJSON TextLogDestination where
  parseJSON =
    Data.withObject
      "TextLogDestination"
      ( \x ->
          TextLogDestination'
            Prelude.<$> (x Data..: "cloudWatch")
      )

instance Prelude.Hashable TextLogDestination where
  hashWithSalt _salt TextLogDestination' {..} =
    _salt `Prelude.hashWithSalt` cloudWatch

instance Prelude.NFData TextLogDestination where
  rnf TextLogDestination' {..} = Prelude.rnf cloudWatch

instance Data.ToJSON TextLogDestination where
  toJSON TextLogDestination' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("cloudWatch" Data..= cloudWatch)]
      )
