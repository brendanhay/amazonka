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
-- Module      : Network.AWS.Budgets.Types.SsmActionDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.SsmActionDefinition where

import Network.AWS.Budgets.Types.ActionSubType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The AWS Systems Manager (SSM) action definition details.
--
-- /See:/ 'newSsmActionDefinition' smart constructor.
data SsmActionDefinition = SsmActionDefinition'
  { -- | The action subType.
    actionSubType :: ActionSubType,
    -- | The Region to run the SSM document.
    region :: Prelude.Text,
    -- | The EC2 and RDS instance IDs.
    instanceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SsmActionDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionSubType', 'ssmActionDefinition_actionSubType' - The action subType.
--
-- 'region', 'ssmActionDefinition_region' - The Region to run the SSM document.
--
-- 'instanceIds', 'ssmActionDefinition_instanceIds' - The EC2 and RDS instance IDs.
newSsmActionDefinition ::
  -- | 'actionSubType'
  ActionSubType ->
  -- | 'region'
  Prelude.Text ->
  -- | 'instanceIds'
  Prelude.NonEmpty Prelude.Text ->
  SsmActionDefinition
newSsmActionDefinition
  pActionSubType_
  pRegion_
  pInstanceIds_ =
    SsmActionDefinition'
      { actionSubType =
          pActionSubType_,
        region = pRegion_,
        instanceIds = Lens._Coerce Lens.# pInstanceIds_
      }

-- | The action subType.
ssmActionDefinition_actionSubType :: Lens.Lens' SsmActionDefinition ActionSubType
ssmActionDefinition_actionSubType = Lens.lens (\SsmActionDefinition' {actionSubType} -> actionSubType) (\s@SsmActionDefinition' {} a -> s {actionSubType = a} :: SsmActionDefinition)

-- | The Region to run the SSM document.
ssmActionDefinition_region :: Lens.Lens' SsmActionDefinition Prelude.Text
ssmActionDefinition_region = Lens.lens (\SsmActionDefinition' {region} -> region) (\s@SsmActionDefinition' {} a -> s {region = a} :: SsmActionDefinition)

-- | The EC2 and RDS instance IDs.
ssmActionDefinition_instanceIds :: Lens.Lens' SsmActionDefinition (Prelude.NonEmpty Prelude.Text)
ssmActionDefinition_instanceIds = Lens.lens (\SsmActionDefinition' {instanceIds} -> instanceIds) (\s@SsmActionDefinition' {} a -> s {instanceIds = a} :: SsmActionDefinition) Prelude.. Lens._Coerce

instance Core.FromJSON SsmActionDefinition where
  parseJSON =
    Core.withObject
      "SsmActionDefinition"
      ( \x ->
          SsmActionDefinition'
            Prelude.<$> (x Core..: "ActionSubType")
            Prelude.<*> (x Core..: "Region")
            Prelude.<*> (x Core..: "InstanceIds")
      )

instance Prelude.Hashable SsmActionDefinition

instance Prelude.NFData SsmActionDefinition

instance Core.ToJSON SsmActionDefinition where
  toJSON SsmActionDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ActionSubType" Core..= actionSubType),
            Prelude.Just ("Region" Core..= region),
            Prelude.Just ("InstanceIds" Core..= instanceIds)
          ]
      )
