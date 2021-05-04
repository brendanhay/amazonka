{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SSM.Types.ParameterInlinePolicy
import Network.AWS.SSM.Types.ParameterTier
import Network.AWS.SSM.Types.ParameterType

-- | Information about parameter usage.
--
-- /See:/ 'newParameterHistory' smart constructor.
data ParameterHistory = ParameterHistory'
  { -- | Date the parameter was last changed or updated.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | Information about the policies assigned to a parameter.
    --
    -- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
    -- in the /AWS Systems Manager User Guide/.
    policies :: Prelude.Maybe [ParameterInlinePolicy],
    -- | Labels assigned to the parameter version.
    labels :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The parameter version.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The name of the parameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | Information about the parameter.
    description :: Prelude.Maybe Prelude.Text,
    -- | The parameter value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The type of parameter used.
    type' :: Prelude.Maybe ParameterType,
    -- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
    -- default is @text@.
    dataType :: Prelude.Maybe Prelude.Text,
    -- | Parameter names can include the following letters and symbols.
    --
    -- a-zA-Z0-9_.-
    allowedPattern :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of the AWS user who last changed the
    -- parameter.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The parameter tier.
    tier :: Prelude.Maybe ParameterTier,
    -- | The ID of the query key used for this parameter.
    keyId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { lastModifiedDate =
        Prelude.Nothing,
      policies = Prelude.Nothing,
      labels = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing,
      value = Prelude.Nothing,
      type' = Prelude.Nothing,
      dataType = Prelude.Nothing,
      allowedPattern = Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      tier = Prelude.Nothing,
      keyId = Prelude.Nothing
    }

-- | Date the parameter was last changed or updated.
parameterHistory_lastModifiedDate :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.UTCTime)
parameterHistory_lastModifiedDate = Lens.lens (\ParameterHistory' {lastModifiedDate} -> lastModifiedDate) (\s@ParameterHistory' {} a -> s {lastModifiedDate = a} :: ParameterHistory) Prelude.. Lens.mapping Prelude._Time

-- | Information about the policies assigned to a parameter.
--
-- <https://docs.aws.amazon.com/systems-manager/latest/userguide/parameter-store-policies.html Assigning parameter policies>
-- in the /AWS Systems Manager User Guide/.
parameterHistory_policies :: Lens.Lens' ParameterHistory (Prelude.Maybe [ParameterInlinePolicy])
parameterHistory_policies = Lens.lens (\ParameterHistory' {policies} -> policies) (\s@ParameterHistory' {} a -> s {policies = a} :: ParameterHistory) Prelude.. Lens.mapping Prelude._Coerce

-- | Labels assigned to the parameter version.
parameterHistory_labels :: Lens.Lens' ParameterHistory (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
parameterHistory_labels = Lens.lens (\ParameterHistory' {labels} -> labels) (\s@ParameterHistory' {} a -> s {labels = a} :: ParameterHistory) Prelude.. Lens.mapping Prelude._Coerce

-- | The parameter version.
parameterHistory_version :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Integer)
parameterHistory_version = Lens.lens (\ParameterHistory' {version} -> version) (\s@ParameterHistory' {} a -> s {version = a} :: ParameterHistory)

-- | The name of the parameter.
parameterHistory_name :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_name = Lens.lens (\ParameterHistory' {name} -> name) (\s@ParameterHistory' {} a -> s {name = a} :: ParameterHistory)

-- | Information about the parameter.
parameterHistory_description :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_description = Lens.lens (\ParameterHistory' {description} -> description) (\s@ParameterHistory' {} a -> s {description = a} :: ParameterHistory)

-- | The parameter value.
parameterHistory_value :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_value = Lens.lens (\ParameterHistory' {value} -> value) (\s@ParameterHistory' {} a -> s {value = a} :: ParameterHistory)

-- | The type of parameter used.
parameterHistory_type :: Lens.Lens' ParameterHistory (Prelude.Maybe ParameterType)
parameterHistory_type = Lens.lens (\ParameterHistory' {type'} -> type') (\s@ParameterHistory' {} a -> s {type' = a} :: ParameterHistory)

-- | The data type of the parameter, such as @text@ or @aws:ec2:image@. The
-- default is @text@.
parameterHistory_dataType :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_dataType = Lens.lens (\ParameterHistory' {dataType} -> dataType) (\s@ParameterHistory' {} a -> s {dataType = a} :: ParameterHistory)

-- | Parameter names can include the following letters and symbols.
--
-- a-zA-Z0-9_.-
parameterHistory_allowedPattern :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_allowedPattern = Lens.lens (\ParameterHistory' {allowedPattern} -> allowedPattern) (\s@ParameterHistory' {} a -> s {allowedPattern = a} :: ParameterHistory)

-- | Amazon Resource Name (ARN) of the AWS user who last changed the
-- parameter.
parameterHistory_lastModifiedUser :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_lastModifiedUser = Lens.lens (\ParameterHistory' {lastModifiedUser} -> lastModifiedUser) (\s@ParameterHistory' {} a -> s {lastModifiedUser = a} :: ParameterHistory)

-- | The parameter tier.
parameterHistory_tier :: Lens.Lens' ParameterHistory (Prelude.Maybe ParameterTier)
parameterHistory_tier = Lens.lens (\ParameterHistory' {tier} -> tier) (\s@ParameterHistory' {} a -> s {tier = a} :: ParameterHistory)

-- | The ID of the query key used for this parameter.
parameterHistory_keyId :: Lens.Lens' ParameterHistory (Prelude.Maybe Prelude.Text)
parameterHistory_keyId = Lens.lens (\ParameterHistory' {keyId} -> keyId) (\s@ParameterHistory' {} a -> s {keyId = a} :: ParameterHistory)

instance Prelude.FromJSON ParameterHistory where
  parseJSON =
    Prelude.withObject
      "ParameterHistory"
      ( \x ->
          ParameterHistory'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "Policies" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Labels")
            Prelude.<*> (x Prelude..:? "Version")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Description")
            Prelude.<*> (x Prelude..:? "Value")
            Prelude.<*> (x Prelude..:? "Type")
            Prelude.<*> (x Prelude..:? "DataType")
            Prelude.<*> (x Prelude..:? "AllowedPattern")
            Prelude.<*> (x Prelude..:? "LastModifiedUser")
            Prelude.<*> (x Prelude..:? "Tier")
            Prelude.<*> (x Prelude..:? "KeyId")
      )

instance Prelude.Hashable ParameterHistory

instance Prelude.NFData ParameterHistory
