{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFormation.SetTypeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies the configuration data for a registered CloudFormation
-- extension, in the given account and region.
--
-- To view the current configuration data for an extension, refer to the
-- @ConfigurationSchema@ element of
-- <AWSCloudFormation/latest/APIReference/API_DescribeType.html DescribeType>.
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/registry-register.html#registry-set-configuration Configuring extensions at the account level>
-- in the /CloudFormation User Guide/.
--
-- It\'s strongly recommended that you use dynamic references to restrict
-- sensitive configuration definitions, such as third-party credentials.
-- For more details on dynamic references, see
-- <https://docs.aws.amazon.com/ Using dynamic references to specify template values>
-- in the /CloudFormation User Guide/.
module Amazonka.CloudFormation.SetTypeConfiguration
  ( -- * Creating a Request
    SetTypeConfiguration (..),
    newSetTypeConfiguration,

    -- * Request Lenses
    setTypeConfiguration_configurationAlias,
    setTypeConfiguration_type,
    setTypeConfiguration_typeArn,
    setTypeConfiguration_typeName,
    setTypeConfiguration_configuration,

    -- * Destructuring the Response
    SetTypeConfigurationResponse (..),
    newSetTypeConfigurationResponse,

    -- * Response Lenses
    setTypeConfigurationResponse_configurationArn,
    setTypeConfigurationResponse_httpStatus,
  )
where

import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetTypeConfiguration' smart constructor.
data SetTypeConfiguration = SetTypeConfiguration'
  { -- | An alias by which to refer to this extension configuration data.
    --
    -- Conditional: Specifying a configuration alias is required when setting a
    -- configuration for a resource type extension.
    configurationAlias :: Prelude.Maybe Prelude.Text,
    -- | The type of extension.
    --
    -- Conditional: You must specify @ConfigurationArn@, or @Type@ and
    -- @TypeName@.
    type' :: Prelude.Maybe ThirdPartyType,
    -- | The Amazon Resource Name (ARN) for the extension, in this account and
    -- region.
    --
    -- For public extensions, this will be the ARN assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
    -- in this account and region. For private extensions, this will be the ARN
    -- assigned when you
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
    -- in this account and region.
    --
    -- Do not include the extension versions suffix at the end of the ARN. You
    -- can set the configuration for an extension, but not for a specific
    -- extension version.
    typeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the extension.
    --
    -- Conditional: You must specify @ConfigurationArn@, or @Type@ and
    -- @TypeName@.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The configuration data for the extension, in this account and region.
    --
    -- The configuration data must be formatted as JSON, and validate against
    -- the schema returned in the @ConfigurationSchema@ response element of
    -- <AWSCloudFormation/latest/APIReference/API_DescribeType.html API_DescribeType>.
    -- For more information, see
    -- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-model.html#resource-type-howto-configuration Defining account-level configuration data for an extension>
    -- in the /CloudFormation CLI User Guide/.
    configuration :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationAlias', 'setTypeConfiguration_configurationAlias' - An alias by which to refer to this extension configuration data.
--
-- Conditional: Specifying a configuration alias is required when setting a
-- configuration for a resource type extension.
--
-- 'type'', 'setTypeConfiguration_type' - The type of extension.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
--
-- 'typeArn', 'setTypeConfiguration_typeArn' - The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
--
-- Do not include the extension versions suffix at the end of the ARN. You
-- can set the configuration for an extension, but not for a specific
-- extension version.
--
-- 'typeName', 'setTypeConfiguration_typeName' - The name of the extension.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
--
-- 'configuration', 'setTypeConfiguration_configuration' - The configuration data for the extension, in this account and region.
--
-- The configuration data must be formatted as JSON, and validate against
-- the schema returned in the @ConfigurationSchema@ response element of
-- <AWSCloudFormation/latest/APIReference/API_DescribeType.html API_DescribeType>.
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-model.html#resource-type-howto-configuration Defining account-level configuration data for an extension>
-- in the /CloudFormation CLI User Guide/.
newSetTypeConfiguration ::
  -- | 'configuration'
  Prelude.Text ->
  SetTypeConfiguration
newSetTypeConfiguration pConfiguration_ =
  SetTypeConfiguration'
    { configurationAlias =
        Prelude.Nothing,
      type' = Prelude.Nothing,
      typeArn = Prelude.Nothing,
      typeName = Prelude.Nothing,
      configuration = pConfiguration_
    }

-- | An alias by which to refer to this extension configuration data.
--
-- Conditional: Specifying a configuration alias is required when setting a
-- configuration for a resource type extension.
setTypeConfiguration_configurationAlias :: Lens.Lens' SetTypeConfiguration (Prelude.Maybe Prelude.Text)
setTypeConfiguration_configurationAlias = Lens.lens (\SetTypeConfiguration' {configurationAlias} -> configurationAlias) (\s@SetTypeConfiguration' {} a -> s {configurationAlias = a} :: SetTypeConfiguration)

-- | The type of extension.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
setTypeConfiguration_type :: Lens.Lens' SetTypeConfiguration (Prelude.Maybe ThirdPartyType)
setTypeConfiguration_type = Lens.lens (\SetTypeConfiguration' {type'} -> type') (\s@SetTypeConfiguration' {} a -> s {type' = a} :: SetTypeConfiguration)

-- | The Amazon Resource Name (ARN) for the extension, in this account and
-- region.
--
-- For public extensions, this will be the ARN assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_ActivateType.html activate the type>
-- in this account and region. For private extensions, this will be the ARN
-- assigned when you
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_RegisterType.html register the type>
-- in this account and region.
--
-- Do not include the extension versions suffix at the end of the ARN. You
-- can set the configuration for an extension, but not for a specific
-- extension version.
setTypeConfiguration_typeArn :: Lens.Lens' SetTypeConfiguration (Prelude.Maybe Prelude.Text)
setTypeConfiguration_typeArn = Lens.lens (\SetTypeConfiguration' {typeArn} -> typeArn) (\s@SetTypeConfiguration' {} a -> s {typeArn = a} :: SetTypeConfiguration)

-- | The name of the extension.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
setTypeConfiguration_typeName :: Lens.Lens' SetTypeConfiguration (Prelude.Maybe Prelude.Text)
setTypeConfiguration_typeName = Lens.lens (\SetTypeConfiguration' {typeName} -> typeName) (\s@SetTypeConfiguration' {} a -> s {typeName = a} :: SetTypeConfiguration)

-- | The configuration data for the extension, in this account and region.
--
-- The configuration data must be formatted as JSON, and validate against
-- the schema returned in the @ConfigurationSchema@ response element of
-- <AWSCloudFormation/latest/APIReference/API_DescribeType.html API_DescribeType>.
-- For more information, see
-- <https://docs.aws.amazon.com/cloudformation-cli/latest/userguide/resource-type-model.html#resource-type-howto-configuration Defining account-level configuration data for an extension>
-- in the /CloudFormation CLI User Guide/.
setTypeConfiguration_configuration :: Lens.Lens' SetTypeConfiguration Prelude.Text
setTypeConfiguration_configuration = Lens.lens (\SetTypeConfiguration' {configuration} -> configuration) (\s@SetTypeConfiguration' {} a -> s {configuration = a} :: SetTypeConfiguration)

instance Core.AWSRequest SetTypeConfiguration where
  type
    AWSResponse SetTypeConfiguration =
      SetTypeConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetTypeConfigurationResult"
      ( \s h x ->
          SetTypeConfigurationResponse'
            Prelude.<$> (x Data..@? "ConfigurationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetTypeConfiguration where
  hashWithSalt _salt SetTypeConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` configurationAlias
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` typeArn
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` configuration

instance Prelude.NFData SetTypeConfiguration where
  rnf SetTypeConfiguration' {..} =
    Prelude.rnf configurationAlias
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf typeArn
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf configuration

instance Data.ToHeaders SetTypeConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetTypeConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery SetTypeConfiguration where
  toQuery SetTypeConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("SetTypeConfiguration" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-15" :: Prelude.ByteString),
        "ConfigurationAlias" Data.=: configurationAlias,
        "Type" Data.=: type',
        "TypeArn" Data.=: typeArn,
        "TypeName" Data.=: typeName,
        "Configuration" Data.=: configuration
      ]

-- | /See:/ 'newSetTypeConfigurationResponse' smart constructor.
data SetTypeConfigurationResponse = SetTypeConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) for the configuration data, in this
    -- account and region.
    --
    -- Conditional: You must specify @ConfigurationArn@, or @Type@ and
    -- @TypeName@.
    configurationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetTypeConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configurationArn', 'setTypeConfigurationResponse_configurationArn' - The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
--
-- 'httpStatus', 'setTypeConfigurationResponse_httpStatus' - The response's http status code.
newSetTypeConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetTypeConfigurationResponse
newSetTypeConfigurationResponse pHttpStatus_ =
  SetTypeConfigurationResponse'
    { configurationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) for the configuration data, in this
-- account and region.
--
-- Conditional: You must specify @ConfigurationArn@, or @Type@ and
-- @TypeName@.
setTypeConfigurationResponse_configurationArn :: Lens.Lens' SetTypeConfigurationResponse (Prelude.Maybe Prelude.Text)
setTypeConfigurationResponse_configurationArn = Lens.lens (\SetTypeConfigurationResponse' {configurationArn} -> configurationArn) (\s@SetTypeConfigurationResponse' {} a -> s {configurationArn = a} :: SetTypeConfigurationResponse)

-- | The response's http status code.
setTypeConfigurationResponse_httpStatus :: Lens.Lens' SetTypeConfigurationResponse Prelude.Int
setTypeConfigurationResponse_httpStatus = Lens.lens (\SetTypeConfigurationResponse' {httpStatus} -> httpStatus) (\s@SetTypeConfigurationResponse' {} a -> s {httpStatus = a} :: SetTypeConfigurationResponse)

instance Prelude.NFData SetTypeConfigurationResponse where
  rnf SetTypeConfigurationResponse' {..} =
    Prelude.rnf configurationArn
      `Prelude.seq` Prelude.rnf httpStatus
