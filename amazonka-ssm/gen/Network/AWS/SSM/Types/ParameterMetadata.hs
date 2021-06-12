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
-- Module      : Network.AWS.SSM.Types.ParameterMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ParameterMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Metadata includes information like the ARN of the last user and the
-- date\/time the parameter was last used.
--
-- /See:/ 'newParameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
  { -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | A list of policies associated with a parameter.
    policies :: Core.Maybe [ParameterInlinePolicy],
    -- | The parameter version.
    version :: Core.Maybe Core.Integer,
    -- | The parameter name.
    name :: Core.Maybe Core.Text,
    -- | Description of the parameter actions.
    description :: Core.Maybe Core.Text,
    -- | The type of parameter. Valid parameter types include the following:
    -- @String@, @StringList@, and @SecureString@.
    type' :: Core.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Core.Maybe Core.Text,
    -- | A parameter name can include only the following letters and symbols.
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
-- Create a value of 'ParameterMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'parameterMetadata_lastModifiedDate' - Date the parameter was last changed or updated.
--
-- 'policies', 'parameterMetadata_policies' - A list of policies associated with a parameter.
--
-- 'version', 'parameterMetadata_version' - The parameter version.
--
-- 'name', 'parameterMetadata_name' - The parameter name.
--
-- 'description', 'parameterMetadata_description' - Description of the parameter actions.
--
-- 'type'', 'parameterMetadata_type' - The type of parameter. Valid parameter types include the following:
-- @String@, @StringList@, and @SecureString@.
--
-- 'dataType', 'parameterMetadata_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'allowedPattern', 'parameterMetadata_allowedPattern' - A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- 'lastModifiedUser', 'parameterMetadata_lastModifiedUser' - Amazon Resource Name (ARN) of the AWS user who last changed the
-- parameter.
--
-- 'tier', 'parameterMetadata_tier' - The parameter tier.
--
-- 'keyId', 'parameterMetadata_keyId' - The ID of the query key used for this parameter.
newParameterMetadata ::
  ParameterMetadata
newParameterMetadata =
  ParameterMetadata'
    { lastModifiedDate = Core.Nothing,
      policies = Core.Nothing,
      version = Core.Nothing,
      name = Core.Nothing,
      description = Core.Nothing,
      type' = Core.Nothing,
      dataType = Core.Nothing,
      allowedPattern = Core.Nothing,
      lastModifiedUser = Core.Nothing,
      tier = Core.Nothing,
      keyId = Core.Nothing
    }

-- | Date the parameter was last changed or updated.
parameterMetadata_lastModifiedDate :: Lens.Lens' ParameterMetadata (Core.Maybe Core.UTCTime)
parameterMetadata_lastModifiedDate = Lens.lens (\ParameterMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@ParameterMetadata' {} a -> s {lastModifiedDate = a} :: ParameterMetadata) Core.. Lens.mapping Core._Time

-- | A list of policies associated with a parameter.
parameterMetadata_policies :: Lens.Lens' ParameterMetadata (Core.Maybe [ParameterInlinePolicy])
parameterMetadata_policies = Lens.lens (\ParameterMetadata' {policies} -> policies) (\s@ParameterMetadata' {} a -> s {policies = a} :: ParameterMetadata) Core.. Lens.mapping Lens._Coerce

-- | The parameter version.
parameterMetadata_version :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Integer)
parameterMetadata_version = Lens.lens (\ParameterMetadata' {version} -> version) (\s@ParameterMetadata' {} a -> s {version = a} :: ParameterMetadata)

-- | The parameter name.
parameterMetadata_name :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_name = Lens.lens (\ParameterMetadata' {name} -> name) (\s@ParameterMetadata' {} a -> s {name = a} :: ParameterMetadata)

-- | Description of the parameter actions.
parameterMetadata_description :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_description = Lens.lens (\ParameterMetadata' {description} -> description) (\s@ParameterMetadata' {} a -> s {description = a} :: ParameterMetadata)

-- | The type of parameter. Valid parameter types include the following:
-- @String@, @StringList@, and @SecureString@.
parameterMetadata_type :: Lens.Lens' ParameterMetadata (Core.Maybe ParameterType)
parameterMetadata_type = Lens.lens (\ParameterMetadata' {type'} -> type') (\s@ParameterMetadata' {} a -> s {type' = a} :: ParameterMetadata)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameterMetadata_dataType :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_dataType = Lens.lens (\ParameterMetadata' {dataType} -> dataType) (\s@ParameterMetadata' {} a -> s {dataType = a} :: ParameterMetadata)

-- | A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
parameterMetadata_allowedPattern :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_allowedPattern = Lens.lens (\ParameterMetadata' {allowedPattern} -> allowedPattern) (\s@ParameterMetadata' {} a -> s {allowedPattern = a} :: ParameterMetadata)

-- | Amazon Resource Name (ARN) of the AWS user who last changed the
-- parameter.
parameterMetadata_lastModifiedUser :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_lastModifiedUser = Lens.lens (\ParameterMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@ParameterMetadata' {} a -> s {lastModifiedUser = a} :: ParameterMetadata)

-- | The parameter tier.
parameterMetadata_tier :: Lens.Lens' ParameterMetadata (Core.Maybe ParameterTier)
parameterMetadata_tier = Lens.lens (\ParameterMetadata' {tier} -> tier) (\s@ParameterMetadata' {} a -> s {tier = a} :: ParameterMetadata)

-- | The ID of the query key used for this parameter.
parameterMetadata_keyId :: Lens.Lens' ParameterMetadata (Core.Maybe Core.Text)
parameterMetadata_keyId = Lens.lens (\ParameterMetadata' {keyId} -> keyId) (\s@ParameterMetadata' {} a -> s {keyId = a} :: ParameterMetadata)

instance Core.FromJSON ParameterMetadata where
  parseJSON =
    Core.withObject
      "ParameterMetadata"
      ( \x ->
          ParameterMetadata'
            Core.<$> (x Core..:? "LastModifiedDate")
            Core.<*> (x Core..:? "Policies" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Description")
            Core.<*> (x Core..:? "Type")
            Core.<*> (x Core..:? "DataType")
            Core.<*> (x Core..:? "AllowedPattern")
            Core.<*> (x Core..:? "LastModifiedUser")
            Core.<*> (x Core..:? "Tier")
            Core.<*> (x Core..:? "KeyId")
      )

instance Core.Hashable ParameterMetadata

instance Core.NFData ParameterMetadata
