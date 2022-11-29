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
-- Module      : Amazonka.CognitoIdentityProvider.Types.RecoveryOptionType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.RecoveryOptionType where

import Amazonka.CognitoIdentityProvider.Types.RecoveryOptionNameType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A map containing a priority as a key, and recovery method name as a
-- value.
--
-- /See:/ 'newRecoveryOptionType' smart constructor.
data RecoveryOptionType = RecoveryOptionType'
  { -- | A positive integer specifying priority of a method with 1 being the
    -- highest priority.
    priority :: Prelude.Natural,
    -- | The recovery method for a user.
    name :: RecoveryOptionNameType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryOptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'recoveryOptionType_priority' - A positive integer specifying priority of a method with 1 being the
-- highest priority.
--
-- 'name', 'recoveryOptionType_name' - The recovery method for a user.
newRecoveryOptionType ::
  -- | 'priority'
  Prelude.Natural ->
  -- | 'name'
  RecoveryOptionNameType ->
  RecoveryOptionType
newRecoveryOptionType pPriority_ pName_ =
  RecoveryOptionType'
    { priority = pPriority_,
      name = pName_
    }

-- | A positive integer specifying priority of a method with 1 being the
-- highest priority.
recoveryOptionType_priority :: Lens.Lens' RecoveryOptionType Prelude.Natural
recoveryOptionType_priority = Lens.lens (\RecoveryOptionType' {priority} -> priority) (\s@RecoveryOptionType' {} a -> s {priority = a} :: RecoveryOptionType)

-- | The recovery method for a user.
recoveryOptionType_name :: Lens.Lens' RecoveryOptionType RecoveryOptionNameType
recoveryOptionType_name = Lens.lens (\RecoveryOptionType' {name} -> name) (\s@RecoveryOptionType' {} a -> s {name = a} :: RecoveryOptionType)

instance Core.FromJSON RecoveryOptionType where
  parseJSON =
    Core.withObject
      "RecoveryOptionType"
      ( \x ->
          RecoveryOptionType'
            Prelude.<$> (x Core..: "Priority")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable RecoveryOptionType where
  hashWithSalt _salt RecoveryOptionType' {..} =
    _salt `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` name

instance Prelude.NFData RecoveryOptionType where
  rnf RecoveryOptionType' {..} =
    Prelude.rnf priority `Prelude.seq` Prelude.rnf name

instance Core.ToJSON RecoveryOptionType where
  toJSON RecoveryOptionType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Priority" Core..= priority),
            Prelude.Just ("Name" Core..= name)
          ]
      )
