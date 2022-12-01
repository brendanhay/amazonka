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
-- Module      : Amazonka.FIS.Types.ActionTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ActionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a target for an action.
--
-- /See:/ 'newActionTarget' smart constructor.
data ActionTarget = ActionTarget'
  { -- | The resource type of the target.
    resourceType :: Prelude.Maybe Prelude.Text
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
-- 'resourceType', 'actionTarget_resourceType' - The resource type of the target.
newActionTarget ::
  ActionTarget
newActionTarget =
  ActionTarget' {resourceType = Prelude.Nothing}

-- | The resource type of the target.
actionTarget_resourceType :: Lens.Lens' ActionTarget (Prelude.Maybe Prelude.Text)
actionTarget_resourceType = Lens.lens (\ActionTarget' {resourceType} -> resourceType) (\s@ActionTarget' {} a -> s {resourceType = a} :: ActionTarget)

instance Core.FromJSON ActionTarget where
  parseJSON =
    Core.withObject
      "ActionTarget"
      ( \x ->
          ActionTarget'
            Prelude.<$> (x Core..:? "resourceType")
      )

instance Prelude.Hashable ActionTarget where
  hashWithSalt _salt ActionTarget' {..} =
    _salt `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ActionTarget where
  rnf ActionTarget' {..} = Prelude.rnf resourceType
