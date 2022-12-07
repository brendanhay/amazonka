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
-- Module      : Amazonka.SESV2.Types.GuardianAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.GuardianAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.FeatureStatus

-- | An object containing additional settings for your VDM configuration as
-- applicable to the Guardian.
--
-- /See:/ 'newGuardianAttributes' smart constructor.
data GuardianAttributes = GuardianAttributes'
  { -- | Specifies the status of your VDM optimized shared delivery. Can be one
    -- of the following:
    --
    -- -   @ENABLED@ – Amazon SES enables optimized shared delivery for your
    --     account.
    --
    -- -   @DISABLED@ – Amazon SES disables optimized shared delivery for your
    --     account.
    optimizedSharedDelivery :: Prelude.Maybe FeatureStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GuardianAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optimizedSharedDelivery', 'guardianAttributes_optimizedSharedDelivery' - Specifies the status of your VDM optimized shared delivery. Can be one
-- of the following:
--
-- -   @ENABLED@ – Amazon SES enables optimized shared delivery for your
--     account.
--
-- -   @DISABLED@ – Amazon SES disables optimized shared delivery for your
--     account.
newGuardianAttributes ::
  GuardianAttributes
newGuardianAttributes =
  GuardianAttributes'
    { optimizedSharedDelivery =
        Prelude.Nothing
    }

-- | Specifies the status of your VDM optimized shared delivery. Can be one
-- of the following:
--
-- -   @ENABLED@ – Amazon SES enables optimized shared delivery for your
--     account.
--
-- -   @DISABLED@ – Amazon SES disables optimized shared delivery for your
--     account.
guardianAttributes_optimizedSharedDelivery :: Lens.Lens' GuardianAttributes (Prelude.Maybe FeatureStatus)
guardianAttributes_optimizedSharedDelivery = Lens.lens (\GuardianAttributes' {optimizedSharedDelivery} -> optimizedSharedDelivery) (\s@GuardianAttributes' {} a -> s {optimizedSharedDelivery = a} :: GuardianAttributes)

instance Data.FromJSON GuardianAttributes where
  parseJSON =
    Data.withObject
      "GuardianAttributes"
      ( \x ->
          GuardianAttributes'
            Prelude.<$> (x Data..:? "OptimizedSharedDelivery")
      )

instance Prelude.Hashable GuardianAttributes where
  hashWithSalt _salt GuardianAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` optimizedSharedDelivery

instance Prelude.NFData GuardianAttributes where
  rnf GuardianAttributes' {..} =
    Prelude.rnf optimizedSharedDelivery

instance Data.ToJSON GuardianAttributes where
  toJSON GuardianAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("OptimizedSharedDelivery" Data..=)
              Prelude.<$> optimizedSharedDelivery
          ]
      )
