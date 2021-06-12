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
-- Module      : Network.AWS.CodePipeline.Types.ActionTypePermissions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionTypePermissions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Details identifying the users with permissions to use the action type.
--
-- /See:/ 'newActionTypePermissions' smart constructor.
data ActionTypePermissions = ActionTypePermissions'
  { -- | A list of AWS account IDs with access to use the action type in their
    -- pipelines.
    allowedAccounts :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ActionTypePermissions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedAccounts', 'actionTypePermissions_allowedAccounts' - A list of AWS account IDs with access to use the action type in their
-- pipelines.
newActionTypePermissions ::
  -- | 'allowedAccounts'
  Core.NonEmpty Core.Text ->
  ActionTypePermissions
newActionTypePermissions pAllowedAccounts_ =
  ActionTypePermissions'
    { allowedAccounts =
        Lens._Coerce Lens.# pAllowedAccounts_
    }

-- | A list of AWS account IDs with access to use the action type in their
-- pipelines.
actionTypePermissions_allowedAccounts :: Lens.Lens' ActionTypePermissions (Core.NonEmpty Core.Text)
actionTypePermissions_allowedAccounts = Lens.lens (\ActionTypePermissions' {allowedAccounts} -> allowedAccounts) (\s@ActionTypePermissions' {} a -> s {allowedAccounts = a} :: ActionTypePermissions) Core.. Lens._Coerce

instance Core.FromJSON ActionTypePermissions where
  parseJSON =
    Core.withObject
      "ActionTypePermissions"
      ( \x ->
          ActionTypePermissions'
            Core.<$> (x Core..: "allowedAccounts")
      )

instance Core.Hashable ActionTypePermissions

instance Core.NFData ActionTypePermissions

instance Core.ToJSON ActionTypePermissions where
  toJSON ActionTypePermissions' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("allowedAccounts" Core..= allowedAccounts)
          ]
      )
