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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionType where

import Network.AWS.CognitoIdentityProvider.Types.RecoveryOptionNameType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A map containing a priority as a key, and recovery method name as a
-- value.
--
-- /See:/ 'newRecoveryOptionType' smart constructor.
data RecoveryOptionType = RecoveryOptionType'
  { -- | A positive integer specifying priority of a method with 1 being the
    -- highest priority.
    priority :: Core.Natural,
    -- | Specifies the recovery method for a user.
    name :: RecoveryOptionNameType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'name', 'recoveryOptionType_name' - Specifies the recovery method for a user.
newRecoveryOptionType ::
  -- | 'priority'
  Core.Natural ->
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
recoveryOptionType_priority :: Lens.Lens' RecoveryOptionType Core.Natural
recoveryOptionType_priority = Lens.lens (\RecoveryOptionType' {priority} -> priority) (\s@RecoveryOptionType' {} a -> s {priority = a} :: RecoveryOptionType)

-- | Specifies the recovery method for a user.
recoveryOptionType_name :: Lens.Lens' RecoveryOptionType RecoveryOptionNameType
recoveryOptionType_name = Lens.lens (\RecoveryOptionType' {name} -> name) (\s@RecoveryOptionType' {} a -> s {name = a} :: RecoveryOptionType)

instance Core.FromJSON RecoveryOptionType where
  parseJSON =
    Core.withObject
      "RecoveryOptionType"
      ( \x ->
          RecoveryOptionType'
            Core.<$> (x Core..: "Priority") Core.<*> (x Core..: "Name")
      )

instance Core.Hashable RecoveryOptionType

instance Core.NFData RecoveryOptionType

instance Core.ToJSON RecoveryOptionType where
  toJSON RecoveryOptionType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Priority" Core..= priority),
            Core.Just ("Name" Core..= name)
          ]
      )
