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
-- Module      : Amazonka.QuickSight.Types.ResourcePermission
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ResourcePermission where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Permission for the resource.
--
-- /See:/ 'newResourcePermission' smart constructor.
data ResourcePermission = ResourcePermission'
  { -- | The Amazon Resource Name (ARN) of the principal. This can be one of the
    -- following:
    --
    -- -   The ARN of an Amazon QuickSight user or group associated with a data
    --     source or dataset. (This is common.)
    --
    -- -   The ARN of an Amazon QuickSight user, group, or namespace associated
    --     with an analysis, dashboard, template, or theme. (This is common.)
    --
    -- -   The ARN of an Amazon Web Services account root: This is an IAM ARN
    --     rather than a QuickSight ARN. Use this option only to share
    --     resources (templates) across Amazon Web Services accounts. (This is
    --     less common.)
    principal :: Prelude.Text,
    -- | The IAM action to grant or revoke permissions on.
    actions :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourcePermission' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'resourcePermission_principal' - The Amazon Resource Name (ARN) of the principal. This can be one of the
-- following:
--
-- -   The ARN of an Amazon QuickSight user or group associated with a data
--     source or dataset. (This is common.)
--
-- -   The ARN of an Amazon QuickSight user, group, or namespace associated
--     with an analysis, dashboard, template, or theme. (This is common.)
--
-- -   The ARN of an Amazon Web Services account root: This is an IAM ARN
--     rather than a QuickSight ARN. Use this option only to share
--     resources (templates) across Amazon Web Services accounts. (This is
--     less common.)
--
-- 'actions', 'resourcePermission_actions' - The IAM action to grant or revoke permissions on.
newResourcePermission ::
  -- | 'principal'
  Prelude.Text ->
  -- | 'actions'
  Prelude.NonEmpty Prelude.Text ->
  ResourcePermission
newResourcePermission pPrincipal_ pActions_ =
  ResourcePermission'
    { principal = pPrincipal_,
      actions = Lens.coerced Lens.# pActions_
    }

-- | The Amazon Resource Name (ARN) of the principal. This can be one of the
-- following:
--
-- -   The ARN of an Amazon QuickSight user or group associated with a data
--     source or dataset. (This is common.)
--
-- -   The ARN of an Amazon QuickSight user, group, or namespace associated
--     with an analysis, dashboard, template, or theme. (This is common.)
--
-- -   The ARN of an Amazon Web Services account root: This is an IAM ARN
--     rather than a QuickSight ARN. Use this option only to share
--     resources (templates) across Amazon Web Services accounts. (This is
--     less common.)
resourcePermission_principal :: Lens.Lens' ResourcePermission Prelude.Text
resourcePermission_principal = Lens.lens (\ResourcePermission' {principal} -> principal) (\s@ResourcePermission' {} a -> s {principal = a} :: ResourcePermission)

-- | The IAM action to grant or revoke permissions on.
resourcePermission_actions :: Lens.Lens' ResourcePermission (Prelude.NonEmpty Prelude.Text)
resourcePermission_actions = Lens.lens (\ResourcePermission' {actions} -> actions) (\s@ResourcePermission' {} a -> s {actions = a} :: ResourcePermission) Prelude.. Lens.coerced

instance Data.FromJSON ResourcePermission where
  parseJSON =
    Data.withObject
      "ResourcePermission"
      ( \x ->
          ResourcePermission'
            Prelude.<$> (x Data..: "Principal")
            Prelude.<*> (x Data..: "Actions")
      )

instance Prelude.Hashable ResourcePermission where
  hashWithSalt _salt ResourcePermission' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` actions

instance Prelude.NFData ResourcePermission where
  rnf ResourcePermission' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf actions

instance Data.ToJSON ResourcePermission where
  toJSON ResourcePermission' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Principal" Data..= principal),
            Prelude.Just ("Actions" Data..= actions)
          ]
      )
