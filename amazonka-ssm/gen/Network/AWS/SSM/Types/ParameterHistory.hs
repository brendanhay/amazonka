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
-- Module      : Network.AWS.SSM.Types.ParameterHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterHistory where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Information about parameter usage.
--
-- /See:/ 'newParameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | Information about the policies assigned to a parameter.
    --
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
    -- in the /AWS Systems Manager User Guide/.
    policies :: Core.Maybe [ParameterInlinePolicy],
    -- | Labels assigned to the parameter version.
    labels :: Core.Maybe (Core.NonEmpty Core.Text),
    -- | The parameter version.
    version :: Core.Maybe Core.Integer,
    -- | The name of the parameter.
    name :: Core.Maybe Core.Text,
    -- | Information about the parameter.
    description :: Core.Maybe Core.Text,
    -- | The parameter value.
    value :: Core.Maybe Core.Text,
    -- | The type of parameter used.
    type' :: Core.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Core.Maybe Core.Text,
    -- | Parameter names can include the following letters and symbols.
    --
    -- a-zA-Z0-9_.-
    allowedPattern :: Core.Maybe Core.Text,
    -- | Amazon Resource Name (ARN) of the AWS user who last changed the
    -- parameter.
    lastModifiedUser :: Core.Maybe Core.Text,
    -- | The parameter tier.
    tier :: Core.Maybe ParameterTier,
    -- | The ID of the query key used for this parameter.
    keyId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ParameterHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'parameterHistory_lastModifiedDate' - Date the parameter was last changed or updated.
--
-- 'policies', 'parameterHistory_policies' - Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
-- in the /AWS Systems Manager User Guide/.
--
-- 'labels', 'parameterHistory_labels' - Labels assigned to the parameter version.
--
-- 'version', 'parameterHistory_version' - The parameter version.
--
-- 'name', 'parameterHistory_name' - The name of the parameter.
--
-- 'description', 'parameterHistory_description' - Information about the parameter.
--
-- 'value', 'parameterHistory_value' - The parameter value.
--
-- 'type'', 'parameterHistory_type' - The type of parameter used.
--
-- 'dataType', 'parameterHistory_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'allowedPattern', 'parameterHistory_allowedPattern' - Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- 'lastModifiedUser', 'parameterHistory_lastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the
-- parameter.
--
-- 'tier', 'parameterHistory_tier' - The parameter tier.
--
-- 'keyId', 'parameterHistory_keyId' - The ID of the query key used for this parameter.
newParameterHistory ::
  ParameterHistory
newParameterHistory =
  ParameterHistory'
    { lastModifiedDate = Core.Nothing,
      policies = Core.Nothing,
      labels = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing,
      dataType = Core.Nothing,
      allowedPattern = Core.Nothing,
      lastModifiedUser = Core.Nothing,
      tier = Core.Nothing,
      keyId = Core.Nothing
    }

-- | Date the parameter was last changed or updated.
parameterHistory_lastModifiedDate :: Lens.Lens' ParameterHistory (Core.Maybe Core.UTCTime)
parameterHistory_lastModifiedDate = Lens.lens (\ParameterHistory' {lastModifiedDate} -> lastModifiedDate) (\s@ParameterHistory' {} a -> s {lastModifiedDate = a} :: ParameterHistory) Core.. Lens.mapping Core._Time

-- | Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
-- in the /AWS Systems Manager User Guide/.
parameterHistory_policies :: Lens.Lens' ParameterHistory (Core.Maybe [ParameterInlinePolicy])
parameterHistory_policies = Lens.lens (\ParameterHistory' {policies} -> policies) (\s@ParameterHistory' {} a -> s {policies = a} :: ParameterHistory) Core.. Lens.mapping Lens._Coerce

-- | Labels assigned to the parameter version.
parameterHistory_labels :: Lens.Lens' ParameterHistory (Core.Maybe (Core.NonEmpty Core.Text))
parameterHistory_labels = Lens.lens (\ParameterHistory' {labels} -> labels) (\s@ParameterHistory' {} a -> s {labels = a} :: ParameterHistory) Core.. Lens.mapping Lens._Coerce

-- | The parameter version.
parameterHistory_version :: Lens.Lens' ParameterHistory (Core.Maybe Core.Integer)
parameterHistory_version = Lens.lens (\ParameterHistory' {version} -> version) (\s@ParameterHistory' {} a -> s {version = a} :: ParameterHistory)

-- | The name of the parameter.
parameterHistory_name :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_name = Lens.lens (\ParameterHistory' {name} -> name) (\s@ParameterHistory' {} a -> s {name = a} :: ParameterHistory)

-- | Information about the parameter.
parameterHistory_description :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_description = Lens.lens (\ParameterHistory' {description} -> description) (\s@ParameterHistory' {} a -> s {description = a} :: ParameterHistory)

-- | The parameter value.
parameterHistory_value :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_value = Lens.lens (\ParameterHistory' {value} -> value) (\s@ParameterHistory' {} a -> s {value = a} :: ParameterHistory)

-- | The type of parameter used.
parameterHistory_type :: Lens.Lens' ParameterHistory (Core.Maybe ParameterType)
parameterHistory_type = Lens.lens (\ParameterHistory' {type'} -> type') (\s@ParameterHistory' {} a -> s {type' = a} :: ParameterHistory)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameterHistory_dataType :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_dataType = Lens.lens (\ParameterHistory' {dataType} -> dataType) (\s@ParameterHistory' {} a -> s {dataType = a} :: ParameterHistory)

-- | Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
parameterHistory_allowedPattern :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_allowedPattern = Lens.lens (\ParameterHistory' {allowedPattern} -> allowedPattern) (\s@ParameterHistory' {} a -> s {allowedPattern = a} :: ParameterHistory)

-- | Amazon Resource Name (ARN) of the AWS user who last changed the
-- parameter.
parameterHistory_lastModifiedUser :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_lastModifiedUser = Lens.lens (\ParameterHistory' {lastModifiedUser} -> lastModifiedUser) (\s@ParameterHistory' {} a -> s {lastModifiedUser = a} :: ParameterHistory)

-- | The parameter tier.
parameterHistory_tier :: Lens.Lens' ParameterHistory (Core.Maybe ParameterTier)
parameterHistory_tier = Lens.lens (\ParameterHistory' {tier} -> tier) (\s@ParameterHistory' {} a -> s {tier = a} :: ParameterHistory)

-- | The ID of the query key used for this parameter.
parameterHistory_keyId :: Lens.Lens' ParameterHistory (Core.Maybe Core.Text)
parameterHistory_keyId = Lens.lens (\ParameterHistory' {keyId} -> keyId) (\s@ParameterHistory' {} a -> s {keyId = a} :: ParameterHistory)

instance Core.FromJSON ParameterHistory where
  parseJSON =
    Core.withObject
      "ParameterHistory"
      ( \x ->
          ParameterHistory'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "Policies" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Labels")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Value")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DataType")
            Core.<*> (x Core..:? "AllowedPattern")
            Core.<*> (x Core..:? "LastModifiedUser")
            Core.<*> (x Core..:? "Tier")
            Core.<*> (x Core..:? "KeyId")
      )

instance Core.Hashable ParameterHistory

instance Core.NFData ParameterHistory
