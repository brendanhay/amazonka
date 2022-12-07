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
-- Module      : Amazonka.SESV2.Types.GuardianOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.GuardianOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.FeatureStatus

-- | An object containing additional settings for your VDM configuration as
-- applicable to the Guardian.
--
-- /See:/ 'newGuardianOptions' smart constructor.
data GuardianOptions = GuardianOptions'
  { -- | Specifies the status of your VDM optimized shared delivery. Can be one
    -- of the following:
    --
    -- -   @ENABLED@ – Amazon SES enables optimized shared delivery for the
    --     configuration set.
    --
    -- -   @DISABLED@ – Amazon SES disables optimized shared delivery for the
    --     configuration set.
    optimizedSharedDelivery :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GuardianOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optimizedSharedDelivery', 'guardianOptions_optimizedSharedDelivery' - Specifies the status of your VDM optimized shared delivery. Can be one
-- of the following:
--
-- -   @ENABLED@ – Amazon SES enables optimized shared delivery for the
--     configuration set.
--
-- -   @DISABLED@ – Amazon SES disables optimized shared delivery for the
--     configuration set.
newGuardianOptions ::
  GuardianOptions
newGuardianOptions =
  GuardianOptions'
    { optimizedSharedDelivery =
        Prelude.Nothing
    }

-- | Specifies the status of your VDM optimized shared delivery. Can be one
-- of the following:
--
-- -   @ENABLED@ – Amazon SES enables optimized shared delivery for the
--     configuration set.
--
-- -   @DISABLED@ – Amazon SES disables optimized shared delivery for the
--     configuration set.
guardianOptions_optimizedSharedDelivery :: Lens.Lens' GuardianOptions (Prelude.Maybe FeatureStatus)
guardianOptions_optimizedSharedDelivery = Lens.lens (\GuardianOptions' {optimizedSharedDelivery} -> optimizedSharedDelivery) (\s@GuardianOptions' {} a -> s {optimizedSharedDelivery = a} :: GuardianOptions)

instance Data.FromJSON GuardianOptions where
  parseJSON =
    Data.withObject
      "GuardianOptions"
      ( \x ->
          GuardianOptions'
            Prelude.<$> (x Data..:? "OptimizedSharedDelivery")
      )

instance Prelude.Hashable GuardianOptions where
  hashWithSalt _salt GuardianOptions' {..} =
    _salt
      `Prelude.hashWithSalt` optimizedSharedDelivery

instance Prelude.NFData GuardianOptions where
  rnf GuardianOptions' {..} =
    Prelude.rnf optimizedSharedDelivery

instance Data.ToJSON GuardianOptions where
  toJSON GuardianOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OptimizedSharedDelivery" Data..=)
              Prelude.<$> optimizedSharedDelivery
          ]
      )
