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
-- Module      : Network.AWS.Backup.Types.ControlScope
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Backup.Types.ControlScope where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A framework consists of one or more controls. Each control has its own
-- control scope. The control scope defines what the control will evaluate.
-- Three examples of control scopes are: a specific backup plan, all backup
-- plans with a specific tag, or all backup plans.
--
-- To set a control scope that includes all of a particular resource, leave
-- the @ControlScope@ empty or do not pass it when calling
-- @CreateFramework@.
--
-- /See:/ 'newControlScope' smart constructor.
data ControlScope = ControlScope'
  { -- | Describes whether the control scope includes one or more types of
    -- resources, such as @EFS@ or @RDS@.
    complianceResourceTypes :: Prelude.Maybe [Prelude.Text],
    -- | Describes whether the control scope includes resources with one or more
    -- tags. Each tag is a key-value pair.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The ID of the only Amazon Web Services resource that you want your
    -- control scope to contain.
    complianceResourceIds :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlScope' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'complianceResourceTypes', 'controlScope_complianceResourceTypes' - Describes whether the control scope includes one or more types of
-- resources, such as @EFS@ or @RDS@.
--
-- 'tags', 'controlScope_tags' - Describes whether the control scope includes resources with one or more
-- tags. Each tag is a key-value pair.
--
-- 'complianceResourceIds', 'controlScope_complianceResourceIds' - The ID of the only Amazon Web Services resource that you want your
-- control scope to contain.
newControlScope ::
  ControlScope
newControlScope =
  ControlScope'
    { complianceResourceTypes =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      complianceResourceIds = Prelude.Nothing
    }

-- | Describes whether the control scope includes one or more types of
-- resources, such as @EFS@ or @RDS@.
controlScope_complianceResourceTypes :: Lens.Lens' ControlScope (Prelude.Maybe [Prelude.Text])
controlScope_complianceResourceTypes = Lens.lens (\ControlScope' {complianceResourceTypes} -> complianceResourceTypes) (\s@ControlScope' {} a -> s {complianceResourceTypes = a} :: ControlScope) Prelude.. Lens.mapping Lens.coerced

-- | Describes whether the control scope includes resources with one or more
-- tags. Each tag is a key-value pair.
controlScope_tags :: Lens.Lens' ControlScope (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
controlScope_tags = Lens.lens (\ControlScope' {tags} -> tags) (\s@ControlScope' {} a -> s {tags = a} :: ControlScope) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the only Amazon Web Services resource that you want your
-- control scope to contain.
controlScope_complianceResourceIds :: Lens.Lens' ControlScope (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
controlScope_complianceResourceIds = Lens.lens (\ControlScope' {complianceResourceIds} -> complianceResourceIds) (\s@ControlScope' {} a -> s {complianceResourceIds = a} :: ControlScope) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ControlScope where
  parseJSON =
    Core.withObject
      "ControlScope"
      ( \x ->
          ControlScope'
            Prelude.<$> ( x Core..:? "ComplianceResourceTypes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ComplianceResourceIds")
      )

instance Prelude.Hashable ControlScope

instance Prelude.NFData ControlScope

instance Core.ToJSON ControlScope where
  toJSON ControlScope' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ComplianceResourceTypes" Core..=)
              Prelude.<$> complianceResourceTypes,
            ("Tags" Core..=) Prelude.<$> tags,
            ("ComplianceResourceIds" Core..=)
              Prelude.<$> complianceResourceIds
          ]
      )
