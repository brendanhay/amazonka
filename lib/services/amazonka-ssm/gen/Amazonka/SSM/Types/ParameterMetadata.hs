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
-- Module      : Amazonka.SSM.Types.ParameterMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParameterMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ParameterInlinePolicy
import Amazonka.SSM.Types.ParameterTier
import Amazonka.SSM.Types.ParameterType

-- | Metadata includes information like the ARN of the last user and the
-- date\/time the parameter was last used.
--
-- /See:/ 'newParameterMetadata' smart constructor.
data ParameterMetadata = ParameterMetadata'
  { -- | A parameter name can include only the following letters and symbols.
    --
    -- a-zA-Z0-9_.-
    allowedPattern :: Prelude.Maybe Prelude.Text,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Description of the parameter actions.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query key used for this parameter.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Name (ARN) of the Amazon Web Services user who last
    -- changed the parameter.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The parameter name.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of policies associated with a parameter.
    policies :: Prelude.Maybe [ParameterInlinePolicy],
    -- | The parameter tier.
    tier :: Prelude.Maybe ParameterTier,
    -- | The type of parameter. Valid parameter types include the following:
    -- @String@, @StringList@, and @SecureString@.
    type' :: Prelude.Maybe ParameterType,
    -- | The parameter version.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPattern', 'parameterMetadata_allowedPattern' - A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- 'dataType', 'parameterMetadata_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'description', 'parameterMetadata_description' - Description of the parameter actions.
--
-- 'keyId', 'parameterMetadata_keyId' - The ID of the query key used for this parameter.
--
-- 'lastModifiedDate', 'parameterMetadata_lastModifiedDate' - Date the parameter was last changed or updated.
--
-- 'lastModifiedUser', 'parameterMetadata_lastModifiedUser' - Amazon Resource Name (ARN) of the Amazon Web Services user who last
-- changed the parameter.
--
-- 'name', 'parameterMetadata_name' - The parameter name.
--
-- 'policies', 'parameterMetadata_policies' - A list of policies associated with a parameter.
--
-- 'tier', 'parameterMetadata_tier' - The parameter tier.
--
-- 'type'', 'parameterMetadata_type' - The type of parameter. Valid parameter types include the following:
-- @String@, @StringList@, and @SecureString@.
--
-- 'version', 'parameterMetadata_version' - The parameter version.
newParameterMetadata ::
  ParameterMetadata
newParameterMetadata =
  ParameterMetadata'
    { allowedPattern =
        Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      keyId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      name = Prelude.Nothing,
      policies = Prelude.Nothing,
      tier = Prelude.Nothing,
      type' = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | A parameter name can include only the following letters and symbols.
--
-- a-zA-Z0-9_.-
parameterMetadata_allowedPattern :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_allowedPattern = Lens.lens (\ParameterMetadata' {allowedPattern} -> allowedPattern) (\s@ParameterMetadata' {} a -> s {allowedPattern = a} :: ParameterMetadata)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameterMetadata_dataType :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_dataType = Lens.lens (\ParameterMetadata' {dataType} -> dataType) (\s@ParameterMetadata' {} a -> s {dataType = a} :: ParameterMetadata)

-- | Description of the parameter actions.
parameterMetadata_description :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_description = Lens.lens (\ParameterMetadata' {description} -> description) (\s@ParameterMetadata' {} a -> s {description = a} :: ParameterMetadata)

-- | The ID of the query key used for this parameter.
parameterMetadata_keyId :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_keyId = Lens.lens (\ParameterMetadata' {keyId} -> keyId) (\s@ParameterMetadata' {} a -> s {keyId = a} :: ParameterMetadata)

-- | Date the parameter was last changed or updated.
parameterMetadata_lastModifiedDate :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.UTCTime)
parameterMetadata_lastModifiedDate = Lens.lens (\ParameterMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@ParameterMetadata' {} a -> s {lastModifiedDate = a} :: ParameterMetadata) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Name (ARN) of the Amazon Web Services user who last
-- changed the parameter.
parameterMetadata_lastModifiedUser :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_lastModifiedUser = Lens.lens (\ParameterMetadata' {lastModifiedUser} -> lastModifiedUser) (\s@ParameterMetadata' {} a -> s {lastModifiedUser = a} :: ParameterMetadata)

-- | The parameter name.
parameterMetadata_name :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Text)
parameterMetadata_name = Lens.lens (\ParameterMetadata' {name} -> name) (\s@ParameterMetadata' {} a -> s {name = a} :: ParameterMetadata)

-- | A list of policies associated with a parameter.
parameterMetadata_policies :: Lens.Lens' ParameterMetadata (Prelude.Maybe [ParameterInlinePolicy])
parameterMetadata_policies = Lens.lens (\ParameterMetadata' {policies} -> policies) (\s@ParameterMetadata' {} a -> s {policies = a} :: ParameterMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The parameter tier.
parameterMetadata_tier :: Lens.Lens' ParameterMetadata (Prelude.Maybe ParameterTier)
parameterMetadata_tier = Lens.lens (\ParameterMetadata' {tier} -> tier) (\s@ParameterMetadata' {} a -> s {tier = a} :: ParameterMetadata)

-- | The type of parameter. Valid parameter types include the following:
-- @String@, @StringList@, and @SecureString@.
parameterMetadata_type :: Lens.Lens' ParameterMetadata (Prelude.Maybe ParameterType)
parameterMetadata_type = Lens.lens (\ParameterMetadata' {type'} -> type') (\s@ParameterMetadata' {} a -> s {type' = a} :: ParameterMetadata)

-- | The parameter version.
parameterMetadata_version :: Lens.Lens' ParameterMetadata (Prelude.Maybe Prelude.Integer)
parameterMetadata_version = Lens.lens (\ParameterMetadata' {version} -> version) (\s@ParameterMetadata' {} a -> s {version = a} :: ParameterMetadata)

instance Data.FromJSON ParameterMetadata where
  parseJSON =
    Data.withObject
      "ParameterMetadata"
      ( \x ->
          ParameterMetadata'
            Prelude.<$> (x Data..:? "AllowedPattern")
            Prelude.<*> (x Data..:? "DataType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "KeyId")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "LastModifiedUser")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Policies" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tier")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable ParameterMetadata where
  hashWithSalt _salt ParameterMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` allowedPattern
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version

instance Prelude.NFData ParameterMetadata where
  rnf ParameterMetadata' {..} =
    Prelude.rnf allowedPattern
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
