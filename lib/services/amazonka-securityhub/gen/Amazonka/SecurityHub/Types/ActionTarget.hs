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
-- Module      : Amazonka.SecurityHub.Types.ActionTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An @ActionTarget@ object.
--
-- /See:/ 'newActionTarget' smart constructor.
data ActionTarget = ActionTarget'
  { -- | The ARN for the target action.
    actionTargetArn :: Prelude.Text,
    -- | The name of the action target.
    name :: Prelude.Text,
    -- | The description of the target action.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionTargetArn', 'actionTarget_actionTargetArn' - The ARN for the target action.
--
-- 'name', 'actionTarget_name' - The name of the action target.
--
-- 'description', 'actionTarget_description' - The description of the target action.
newActionTarget ::
  -- | 'actionTargetArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  ActionTarget
newActionTarget
  pActionTargetArn_
  pName_
  pDescription_ =
    ActionTarget'
      { actionTargetArn = pActionTargetArn_,
        name = pName_,
        description = pDescription_
      }

-- | The ARN for the target action.
actionTarget_actionTargetArn :: Lens.Lens' ActionTarget Prelude.Text
actionTarget_actionTargetArn = Lens.lens (\ActionTarget' {actionTargetArn} -> actionTargetArn) (\s@ActionTarget' {} a -> s {actionTargetArn = a} :: ActionTarget)

-- | The name of the action target.
actionTarget_name :: Lens.Lens' ActionTarget Prelude.Text
actionTarget_name = Lens.lens (\ActionTarget' {name} -> name) (\s@ActionTarget' {} a -> s {name = a} :: ActionTarget)

-- | The description of the target action.
actionTarget_description :: Lens.Lens' ActionTarget Prelude.Text
actionTarget_description = Lens.lens (\ActionTarget' {description} -> description) (\s@ActionTarget' {} a -> s {description = a} :: ActionTarget)

instance Core.FromJSON ActionTarget where
  parseJSON =
    Core.withObject
      "ActionTarget"
      ( \x ->
          ActionTarget'
            Prelude.<$> (x Core..: "ActionTargetArn")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Description")
      )

instance Prelude.Hashable ActionTarget where
  hashWithSalt _salt ActionTarget' {..} =
    _salt `Prelude.hashWithSalt` actionTargetArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description

instance Prelude.NFData ActionTarget where
  rnf ActionTarget' {..} =
    Prelude.rnf actionTargetArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
