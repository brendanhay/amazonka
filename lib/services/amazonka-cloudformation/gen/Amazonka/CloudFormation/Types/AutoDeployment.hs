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
-- Module      : Amazonka.CloudFormation.Types.AutoDeployment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.AutoDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | [Service-managed permissions] Describes whether StackSets automatically
-- deploys to Organizations accounts that are added to a target
-- organization or organizational unit (OU).
--
-- /See:/ 'newAutoDeployment' smart constructor.
data AutoDeployment = AutoDeployment'
  { -- | If set to @true@, StackSets automatically deploys additional stack
    -- instances to Organizations accounts that are added to a target
    -- organization or organizational unit (OU) in the specified Regions. If an
    -- account is removed from a target organization or OU, StackSets deletes
    -- stack instances from the account in the specified Regions.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | If set to @true@, stack resources are retained when an account is
    -- removed from a target organization or OU. If set to @false@, stack
    -- resources are deleted. Specify only if @Enabled@ is set to @True@.
    retainStacksOnAccountRemoval :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enabled', 'autoDeployment_enabled' - If set to @true@, StackSets automatically deploys additional stack
-- instances to Organizations accounts that are added to a target
-- organization or organizational unit (OU) in the specified Regions. If an
-- account is removed from a target organization or OU, StackSets deletes
-- stack instances from the account in the specified Regions.
--
-- 'retainStacksOnAccountRemoval', 'autoDeployment_retainStacksOnAccountRemoval' - If set to @true@, stack resources are retained when an account is
-- removed from a target organization or OU. If set to @false@, stack
-- resources are deleted. Specify only if @Enabled@ is set to @True@.
newAutoDeployment ::
  AutoDeployment
newAutoDeployment =
  AutoDeployment'
    { enabled = Prelude.Nothing,
      retainStacksOnAccountRemoval = Prelude.Nothing
    }

-- | If set to @true@, StackSets automatically deploys additional stack
-- instances to Organizations accounts that are added to a target
-- organization or organizational unit (OU) in the specified Regions. If an
-- account is removed from a target organization or OU, StackSets deletes
-- stack instances from the account in the specified Regions.
autoDeployment_enabled :: Lens.Lens' AutoDeployment (Prelude.Maybe Prelude.Bool)
autoDeployment_enabled = Lens.lens (\AutoDeployment' {enabled} -> enabled) (\s@AutoDeployment' {} a -> s {enabled = a} :: AutoDeployment)

-- | If set to @true@, stack resources are retained when an account is
-- removed from a target organization or OU. If set to @false@, stack
-- resources are deleted. Specify only if @Enabled@ is set to @True@.
autoDeployment_retainStacksOnAccountRemoval :: Lens.Lens' AutoDeployment (Prelude.Maybe Prelude.Bool)
autoDeployment_retainStacksOnAccountRemoval = Lens.lens (\AutoDeployment' {retainStacksOnAccountRemoval} -> retainStacksOnAccountRemoval) (\s@AutoDeployment' {} a -> s {retainStacksOnAccountRemoval = a} :: AutoDeployment)

instance Data.FromXML AutoDeployment where
  parseXML x =
    AutoDeployment'
      Prelude.<$> (x Data..@? "Enabled")
      Prelude.<*> (x Data..@? "RetainStacksOnAccountRemoval")

instance Prelude.Hashable AutoDeployment where
  hashWithSalt _salt AutoDeployment' {..} =
    _salt
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` retainStacksOnAccountRemoval

instance Prelude.NFData AutoDeployment where
  rnf AutoDeployment' {..} =
    Prelude.rnf enabled
      `Prelude.seq` Prelude.rnf retainStacksOnAccountRemoval

instance Data.ToQuery AutoDeployment where
  toQuery AutoDeployment' {..} =
    Prelude.mconcat
      [ "Enabled" Data.=: enabled,
        "RetainStacksOnAccountRemoval"
          Data.=: retainStacksOnAccountRemoval
      ]
