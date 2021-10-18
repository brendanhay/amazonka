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
-- Module      : Network.AWS.CloudFormation.Types.TypeConfigurationDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.TypeConfigurationDetails where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information concerning the specification of a CloudFormation
-- extension in a given account and region.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newTypeConfigurationDetails' smart constructor.
data TypeConfigurationDetails = TypeConfigurationDetails'
  { -- | The name of the extension.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The alias specified for this configuration, if one was specified when
    -- the configuration was set.
    alias :: Prelude.Maybe Prelude.Text,
    -- | A JSON string specifying the configuration data for the extension, in
    -- this account and region.
    --
    -- If a configuration has not been set for a specified extension,
    -- CloudFormation returns @{}@.
    configuration :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the configuration data, in this
    -- account and region.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the configuration data was last updated for this extension.
    --
    -- If a configuration has not been set for a specified extension,
    -- CloudFormation returns @null@.
    lastUpdated :: Prelude.Maybe Core.ISO8601,
    -- | Whether or not this configuration data is the default configuration for
    -- the extension.
    isDefaultConfiguration :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) for the extension, in this account and
    -- region.
    --
    -- For public extensions, this will be the ARN assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
    -- in this account and region. For private extensions, this will be the ARN
    -- assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
    -- in this account and region.
    typeArn :: Prelude.Maybe Prelude.Text
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
-- 'typeName', 'typeConfigurationDetails_typeName' - The name of the extension.
--
-- 'alias', 'typeConfigurationDetails_alias' - The alias specified for this configuration, if one was specified when
-- the configuration was set.
--
-- 'configuration', 'typeConfigurationDetails_configuration' - A JSON string specifying the configuration data for the extension, in
-- this account and region.
--
-- If a configuration has not been set for a specified extension,
-- CloudFormation returns @{}@.
--
-- 'arn', 'typeConfigurationDetails_arn' - The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
--
-- 'lastUpdated', 'typeConfigurationDetails_lastUpdated' - When the configuration data was last updated for this extension.
--
-- If a configuration has not been set for a specified extension,
-- CloudFormation returns @null@.
--
-- 'isDefaultConfiguration', 'typeConfigurationDetails_isDefaultConfiguration' - Whether or not this configuration data is the default configuration for
-- the extension.
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
newTypeConfigurationDetails ::
  TypeConfigurationDetails
newTypeConfigurationDetails =
  TypeConfigurationDetails'
    { typeName =
        Prelude.Nothing,
      alias = Prelude.Nothing,
      configuration = Prelude.Nothing,
      arn = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      isDefaultConfiguration = Prelude.Nothing,
      typeArn = Prelude.Nothing
    }

-- | The name of the extension.
typeConfigurationDetails_typeName :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_typeName = Lens.lens (\TypeConfigurationDetails' {typeName} -> typeName) (\s@TypeConfigurationDetails' {} a -> s {typeName = a} :: TypeConfigurationDetails)

-- | The alias specified for this configuration, if one was specified when
-- the configuration was set.
typeConfigurationDetails_alias :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_alias = Lens.lens (\TypeConfigurationDetails' {alias} -> alias) (\s@TypeConfigurationDetails' {} a -> s {alias = a} :: TypeConfigurationDetails)

-- | A JSON string specifying the configuration data for the extension, in
-- this account and region.
--
-- If a configuration has not been set for a specified extension,
-- CloudFormation returns @{}@.
typeConfigurationDetails_configuration :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_configuration = Lens.lens (\TypeConfigurationDetails' {configuration} -> configuration) (\s@TypeConfigurationDetails' {} a -> s {configuration = a} :: TypeConfigurationDetails)

-- | The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
typeConfigurationDetails_arn :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Text)
typeConfigurationDetails_arn = Lens.lens (\TypeConfigurationDetails' {arn} -> arn) (\s@TypeConfigurationDetails' {} a -> s {arn = a} :: TypeConfigurationDetails)

-- | When the configuration data was last updated for this extension.
--
-- If a configuration has not been set for a specified extension,
-- CloudFormation returns @null@.
typeConfigurationDetails_lastUpdated :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.UTCTime)
typeConfigurationDetails_lastUpdated = Lens.lens (\TypeConfigurationDetails' {lastUpdated} -> lastUpdated) (\s@TypeConfigurationDetails' {} a -> s {lastUpdated = a} :: TypeConfigurationDetails) Prelude.. Lens.mapping Core._Time

-- | Whether or not this configuration data is the default configuration for
-- the extension.
typeConfigurationDetails_isDefaultConfiguration :: Lens.Lens' TypeConfigurationDetails (Prelude.Maybe Prelude.Bool)
typeConfigurationDetails_isDefaultConfiguration = Lens.lens (\TypeConfigurationDetails' {isDefaultConfiguration} -> isDefaultConfiguration) (\s@TypeConfigurationDetails' {} a -> s {isDefaultConfiguration = a} :: TypeConfigurationDetails)

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

instance Core.FromXML TypeConfigurationDetails where
  parseXML x =
    TypeConfigurationDetails'
      Prelude.<$> (x Core..@? "TypeName")
      Prelude.<*> (x Core..@? "Alias")
      Prelude.<*> (x Core..@? "Configuration")
      Prelude.<*> (x Core..@? "Arn")
      Prelude.<*> (x Core..@? "LastUpdated")
      Prelude.<*> (x Core..@? "IsDefaultConfiguration")
      Prelude.<*> (x Core..@? "TypeArn")

instance Prelude.Hashable TypeConfigurationDetails

instance Prelude.NFData TypeConfigurationDetails
