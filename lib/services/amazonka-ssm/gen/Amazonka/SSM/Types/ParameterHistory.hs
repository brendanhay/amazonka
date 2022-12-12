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
-- Module      : Amazonka.SSM.Types.ParameterHistory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParameterHistory where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.ParameterInlinePolicy
import Amazonka.SSM.Types.ParameterTier
import Amazonka.SSM.Types.ParameterType

-- | Information about parameter usage.
--
-- /See:/ 'newParameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { -- | Parameter names can include the following letters and symbols.
    --
    -- a-zA-Z0-9_.-
    allowedPattern :: Prelude.Maybe Prelude.Text,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Information about the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the query key used for this parameter.
    keyId :: Prelude.Maybe Prelude.Text,
    -- | Labels assigned to the parameter version.
    labels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | Amazon Resource Name (ARN) of the Amazon Web Services user who last
    -- changed the parameter.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The name of the parameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the policies assigned to a parameter.
    --
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
    -- in the /Amazon Web Services Systems Manager User Guide/.
    policies :: Prelude.Maybe [ParameterInlinePolicy],
    -- | The parameter tier.
    tier :: Prelude.Maybe ParameterTier,
    -- | The type of parameter used.
    type' :: Prelude.Maybe ParameterType,
    -- | The parameter value.
    value :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The parameter version.
    version :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowedPattern', 'parameterHistory_allowedPattern' - Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
--
-- 'dataType', 'parameterHistory_dataType' - The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
--
-- 'description', 'parameterHistory_description' - Information about the parameter.
--
-- 'keyId', 'parameterHistory_keyId' - The ID of the query key used for this parameter.
--
-- 'labels', 'parameterHistory_labels' - Labels assigned to the parameter version.
--
-- 'lastModifiedDate', 'parameterHistory_lastModifiedDate' - Date the parameter was last changed or updated.
--
-- 'lastModifiedUser', 'parameterHistory_lastModifiedUser' - Amazon Resource Name (ARN) of the Amazon Web Services user who last
-- changed the parameter.
--
-- 'name', 'parameterHistory_name' - The name of the parameter.
--
-- 'policies', 'parameterHistory_policies' - Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
-- in the /Amazon Web Services Systems Manager User Guide/.
--
-- 'tier', 'parameterHistory_tier' - The parameter tier.
--
-- 'type'', 'parameterHistory_type' - The type of parameter used.
--
-- 'value', 'parameterHistory_value' - The parameter value.
--
-- 'version', 'parameterHistory_version' - The parameter version.
newParameterHistory ::
  ParameterHistory
newParameterHistory =
  ParameterHistory'
    { allowedPattern = Prelude.Nothing,
      dataType = Prelude.Nothing,
      description = Prelude.Nothing,
      keyId = Prelude.Nothing,
      labels = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      name = Prelude.Nothing,
      policies = Prelude.Nothing,
      tier = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
parameterHistory_allowedPattern :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_allowedPattern = Lens.lens (\ParameterHistory' {allowedPattern} -> allowedPattern) (\s@ParameterHistory' {} a -> s {allowedPattern = a} :: ParameterHistory)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameterHistory_dataType :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_dataType = Lens.lens (\ParameterHistory' {dataType} -> dataType) (\s@ParameterHistory' {} a -> s {dataType = a} :: ParameterHistory)

-- | Information about the parameter.
parameterHistory_description :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_description = Lens.lens (\ParameterHistory' {description} -> description) (\s@ParameterHistory' {} a -> s {description = a} :: ParameterHistory)

-- | The ID of the query key used for this parameter.
parameterHistory_keyId :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_keyId = Lens.lens (\ParameterHistory' {keyId} -> keyId) (\s@ParameterHistory' {} a -> s {keyId = a} :: ParameterHistory)

-- | Labels assigned to the parameter version.
parameterHistory_labels :: Lens.Lens' ParameterHistory (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
parameterHistory_labels = Lens.lens (\ParameterHistory' {labels} -> labels) (\s@ParameterHistory' {} a -> s {labels = a} :: ParameterHistory) Prelude.. Lens.mapping Lens.coerced

-- | Date the parameter was last changed or updated.
parameterHistory_lastModifiedDate :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.UTCTime)
parameterHistory_lastModifiedDate = Lens.lens (\ParameterHistory' {lastModifiedDate} -> lastModifiedDate) (\s@ParameterHistory' {} a -> s {lastModifiedDate = a} :: ParameterHistory) Prelude.. Lens.mapping Data._Time

-- | Amazon Resource Name (ARN) of the Amazon Web Services user who last
-- changed the parameter.
parameterHistory_lastModifiedUser :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_lastModifiedUser = Lens.lens (\ParameterHistory' {lastModifiedUser} -> lastModifiedUser) (\s@ParameterHistory' {} a -> s {lastModifiedUser = a} :: ParameterHistory)

-- | The name of the parameter.
parameterHistory_name :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_name = Lens.lens (\ParameterHistory' {name} -> name) (\s@ParameterHistory' {} a -> s {name = a} :: ParameterHistory)

-- | Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
-- in the /Amazon Web Services Systems Manager User Guide/.
parameterHistory_policies :: Lens.Lens' ParameterHistory (Prelude.Maybe [ParameterInlinePolicy])
parameterHistory_policies = Lens.lens (\ParameterHistory' {policies} -> policies) (\s@ParameterHistory' {} a -> s {policies = a} :: ParameterHistory) Prelude.. Lens.mapping Lens.coerced

-- | The parameter tier.
parameterHistory_tier :: Lens.Lens' ParameterHistory (Prelude.Maybe ParameterTier)
parameterHistory_tier = Lens.lens (\ParameterHistory' {tier} -> tier) (\s@ParameterHistory' {} a -> s {tier = a} :: ParameterHistory)

-- | The type of parameter used.
parameterHistory_type :: Lens.Lens' ParameterHistory (Prelude.Maybe ParameterType)
parameterHistory_type = Lens.lens (\ParameterHistory' {type'} -> type') (\s@ParameterHistory' {} a -> s {type' = a} :: ParameterHistory)

-- | The parameter value.
parameterHistory_value :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_value = Lens.lens (\ParameterHistory' {value} -> value) (\s@ParameterHistory' {} a -> s {value = a} :: ParameterHistory) Prelude.. Lens.mapping Data._Sensitive

-- | The parameter version.
parameterHistory_version :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Integer)
parameterHistory_version = Lens.lens (\ParameterHistory' {version} -> version) (\s@ParameterHistory' {} a -> s {version = a} :: ParameterHistory)

instance Data.FromJSON ParameterHistory where
  parseJSON =
    Data.withObject
      "ParameterHistory"
      ( \x ->
          ParameterHistory'
            Prelude.<$> (x Data..:? "AllowedPattern")
            Prelude.<*> (x Data..:? "DataType")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "KeyId")
            Prelude.<*> (x Data..:? "Labels")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "LastModifiedUser")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Policies" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Tier")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
            Prelude.<*> (x Data..:? "Version")
      )

instance Prelude.Hashable ParameterHistory where
  hashWithSalt _salt ParameterHistory' {..} =
    _salt `Prelude.hashWithSalt` allowedPattern
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` keyId
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` tier
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` version

instance Prelude.NFData ParameterHistory where
  rnf ParameterHistory' {..} =
    Prelude.rnf allowedPattern
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf keyId
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf tier
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
      `Prelude.seq` Prelude.rnf version
