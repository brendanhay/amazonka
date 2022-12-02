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
-- Module      : Amazonka.CloudFormation.Types.TypeConfigurationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.TypeConfigurationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information concerning the specification of a CloudFormation
-- extension in a given account and region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newTypeConfigurationDetails' smart constructor.
data TypeConfigurationDetails = TypeConfigurationDetails'
  { -- | The alias specified for this configuration, if one was specified when
    -- the configuration was set.
    alias :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the extension, in this account and
    -- region.
    --
    -- For public extensions, this will be the ARN assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
    -- in this account and region. For private extensions, this will be the ARN
    -- assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
    -- in this account and region.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | A JSON string specifying the configuration data for the extension, in
    -- this account and region.
    --
    -- If a configuration hasn\'t been set for a specified extension,
    -- CloudFormation returns @{}@.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the configuration data, in this
    -- account and region.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | Whether this configuration data is the default configuration for the
    -- extension.
    isDefaultConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | When the configuration data was last updated for this extension.
    --
    -- If a configuration hasn\'t been set for a specified extension,
    -- CloudFormation returns @null@.
    lastUpdated :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypeConfigurationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alias', 'typeConfigurationDetails_alias' - The alias specified for this configuration, if one was specified when
-- the configuration was set.
--
-- 'typeArn', 'typeConfigurationDetails_typeArn' - The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
--
-- 'configuration', 'typeConfigurationDetails_configuration' - A JSON string specifying the configuration data for the extension, in
-- this account and region.
--
-- If a configuration hasn\'t been set for a specified extension,
-- CloudFormation returns @{}@.
--
-- 'arn', 'typeConfigurationDetails_arn' - The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
--
-- 'typeName', 'typeConfigurationDetails_typeName' - The name of the extension.
--
-- 'isDefaultConfiguration', 'typeConfigurationDetails_isDefaultConfiguration' - Whether this configuration data is the default configuration for the
-- extension.
--
-- 'lastUpdated', 'typeConfigurationDetails_lastUpdated' - When the configuration data was last updated for this extension.
--
-- If a configuration hasn\'t been set for a specified extension,
-- CloudFormation returns @null@.
newTypeConfigurationDetails ::
  TypeConfigurationDetails
newTypeConfigurationDetails =
  TypeConfigurationDetails'
    { alias = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      typeName = Prelude.Nothing,
      isDefaultConfiguration = Prelude.Nothing,
      lastUpdated = Prelude.Nothing
    }

-- | The alias specified for this configuration, if one was specified when
-- the configuration was set.
typeConfigurationDetails_alias :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_alias = Lens.lens (\TypeConfigurationDetails' {alias} -> alias) (\s@TypeConfigurationDetails' {} a -> s {alias = a} :: TypeConfigurationDetails)

-- | The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
typeConfigurationDetails_typeArn :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_typeArn = Lens.lens (\TypeConfigurationDetails' {typeArn} -> typeArn) (\s@TypeConfigurationDetails' {} a -> s {typeArn = a} :: TypeConfigurationDetails)

-- | A JSON string specifying the configuration data for the extension, in
-- this account and region.
--
-- If a configuration hasn\'t been set for a specified extension,
-- CloudFormation returns @{}@.
typeConfigurationDetails_configuration :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_configuration = Lens.lens (\TypeConfigurationDetails' {configuration} -> configuration) (\s@TypeConfigurationDetails' {} a -> s {configuration = a} :: TypeConfigurationDetails)

-- | The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
typeConfigurationDetails_arn :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_arn = Lens.lens (\TypeConfigurationDetails' {arn} -> arn) (\s@TypeConfigurationDetails' {} a -> s {arn = a} :: TypeConfigurationDetails)

-- | The name of the extension.
typeConfigurationDetails_typeName :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_typeName = Lens.lens (\TypeConfigurationDetails' {typeName} -> typeName) (\s@TypeConfigurationDetails' {} a -> s {typeName = a} :: TypeConfigurationDetails)

-- | Whether this configuration data is the default configuration for the
-- extension.
typeConfigurationDetails_isDefaultConfiguration :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Bool)
typeConfigurationDetails_isDefaultConfiguration = Lens.lens (\TypeConfigurationDetails' {isDefaultConfiguration} -> isDefaultConfiguration) (\s@TypeConfigurationDetails' {} a -> s {isDefaultConfiguration = a} :: TypeConfigurationDetails)

-- | When the configuration data was last updated for this extension.
--
-- If a configuration hasn\'t been set for a specified extension,
-- CloudFormation returns @null@.
typeConfigurationDetails_lastUpdated :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.UTCTime)
typeConfigurationDetails_lastUpdated = Lens.lens (\TypeConfigurationDetails' {lastUpdated} -> lastUpdated) (\s@TypeConfigurationDetails' {} a -> s {lastUpdated = a} :: TypeConfigurationDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromXML TypeConfigurationDetails where
  parseXML x =
    TypeConfigurationDetails'
      Prelude.<$> (x Data..@? "Alias")
      Prelude.<*> (x Data..@? "TypeArn")
      Prelude.<*> (x Data..@? "Configuration")
      Prelude.<*> (x Data..@? "Arn")
      Prelude.<*> (x Data..@? "TypeName")
      Prelude.<*> (x Data..@? "IsDefaultConfiguration")
      Prelude.<*> (x Data..@? "LastUpdated")

instance Prelude.Hashable TypeConfigurationDetails where
  hashWithSalt _salt TypeConfigurationDetails' {..} =
    _salt `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` typeArn
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` isDefaultConfiguration
      `Prelude.hashWithSalt` lastUpdated

instance Prelude.NFData TypeConfigurationDetails where
  rnf TypeConfigurationDetails' {..} =
    Prelude.rnf alias
      `Prelude.seq` Prelude.rnf typeArn
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf isDefaultConfiguration
      `Prelude.seq` Prelude.rnf lastUpdated
