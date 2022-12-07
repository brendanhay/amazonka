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
-- Module      : Amazonka.FMS.Types.ActionTarget
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.ActionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a remediation action target.
--
-- /See:/ 'newActionTarget' smart constructor.
data ActionTarget = ActionTarget'
  { -- | The ID of the remediation target.
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | A description of the remediation action target.
    description :: Prelude.Maybe Prelude.Text
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
-- 'resourceId', 'actionTarget_resourceId' - The ID of the remediation target.
--
-- 'description', 'actionTarget_description' - A description of the remediation action target.
newActionTarget ::
  ActionTarget
newActionTarget =
  ActionTarget'
    { resourceId = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ID of the remediation target.
actionTarget_resourceId :: Lens.Lens' ActionTarget (Prelude.Maybe Prelude.Text)
actionTarget_resourceId = Lens.lens (\ActionTarget' {resourceId} -> resourceId) (\s@ActionTarget' {} a -> s {resourceId = a} :: ActionTarget)

-- | A description of the remediation action target.
actionTarget_description :: Lens.Lens' ActionTarget (Prelude.Maybe Prelude.Text)
actionTarget_description = Lens.lens (\ActionTarget' {description} -> description) (\s@ActionTarget' {} a -> s {description = a} :: ActionTarget)

instance Data.FromJSON ActionTarget where
  parseJSON =
    Data.withObject
      "ActionTarget"
      ( \x ->
          ActionTarget'
            Prelude.<$> (x Data..:? "ResourceId")
            Prelude.<*> (x Data..:? "Description")
      )

instance Prelude.Hashable ActionTarget where
  hashWithSalt _salt ActionTarget' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` description

instance Prelude.NFData ActionTarget where
  rnf ActionTarget' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf description
