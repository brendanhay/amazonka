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
-- Module      : Amazonka.LicenseManager.CreateLicenseConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a license configuration.
--
-- A license configuration is an abstraction of a customer license
-- agreement that can be consumed and enforced by License Manager.
-- Components include specifications for the license type (licensing by
-- instance, socket, CPU, or vCPU), allowed tenancy (shared tenancy,
-- Dedicated Instance, Dedicated Host, or all of these), license affinity
-- to host (how long a license must be associated with a host), and the
-- number of licenses purchased and used.
module Amazonka.LicenseManager.CreateLicenseConfiguration
  ( -- * Creating a Request
    CreateLicenseConfiguration (..),
    newCreateLicenseConfiguration,

    -- * Request Lenses
    createLicenseConfiguration_description,
    createLicenseConfiguration_disassociateWhenNotFound,
    createLicenseConfiguration_licenseCount,
    createLicenseConfiguration_licenseCountHardLimit,
    createLicenseConfiguration_licenseRules,
    createLicenseConfiguration_productInformationList,
    createLicenseConfiguration_tags,
    createLicenseConfiguration_name,
    createLicenseConfiguration_licenseCountingType,

    -- * Destructuring the Response
    CreateLicenseConfigurationResponse (..),
    newCreateLicenseConfigurationResponse,

    -- * Response Lenses
    createLicenseConfigurationResponse_licenseConfigurationArn,
    createLicenseConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLicenseConfiguration' smart constructor.
data CreateLicenseConfiguration = CreateLicenseConfiguration'
  { -- | Description of the license configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | When true, disassociates a resource when software is uninstalled.
    disassociateWhenNotFound :: Prelude.Maybe Prelude.Bool,
    -- | Number of licenses managed by the license configuration.
    licenseCount :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether hard or soft license enforcement is used. Exceeding a
    -- hard limit blocks the launch of new instances.
    licenseCountHardLimit :: Prelude.Maybe Prelude.Bool,
    -- | License rules. The syntax is #name=value (for example,
    -- #allowedTenancy=EC2-DedicatedHost). The available rules vary by
    -- dimension, as follows.
    --
    -- -   @Cores@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
    --     @maximumCores@ | @minimumCores@
    --
    -- -   @Instances@ dimension: @allowedTenancy@ | @maximumCores@ |
    --     @minimumCores@ | @maximumSockets@ | @minimumSockets@ |
    --     @maximumVcpus@ | @minimumVcpus@
    --
    -- -   @Sockets@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
    --     @maximumSockets@ | @minimumSockets@
    --
    -- -   @vCPUs@ dimension: @allowedTenancy@ | @honorVcpuOptimization@ |
    --     @maximumVcpus@ | @minimumVcpus@
    --
    -- The unit for @licenseAffinityToHost@ is days and the range is 1 to 180.
    -- The possible values for @allowedTenancy@ are @EC2-Default@,
    -- @EC2-DedicatedHost@, and @EC2-DedicatedInstance@. The possible values
    -- for @honorVcpuOptimization@ are @True@ and @False@.
    licenseRules :: Prelude.Maybe [Prelude.Text],
    -- | Product information.
    productInformationList :: Prelude.Maybe [ProductInformation],
    -- | Tags to add to the license configuration.
    tags :: Prelude.Maybe [Tag],
    -- | Name of the license configuration.
    name :: Prelude.Text,
    -- | Dimension used to track the license inventory.
    licenseCountingType :: LicenseCountingType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createLicenseConfiguration_description' - Description of the license configuration.
--
-- 'disassociateWhenNotFound', 'createLicenseConfiguration_disassociateWhenNotFound' - When true, disassociates a resource when software is uninstalled.
--
-- 'licenseCount', 'createLicenseConfiguration_licenseCount' - Number of licenses managed by the license configuration.
--
-- 'licenseCountHardLimit', 'createLicenseConfiguration_licenseCountHardLimit' - Indicates whether hard or soft license enforcement is used. Exceeding a
-- hard limit blocks the launch of new instances.
--
-- 'licenseRules', 'createLicenseConfiguration_licenseRules' - License rules. The syntax is #name=value (for example,
-- #allowedTenancy=EC2-DedicatedHost). The available rules vary by
-- dimension, as follows.
--
-- -   @Cores@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
--     @maximumCores@ | @minimumCores@
--
-- -   @Instances@ dimension: @allowedTenancy@ | @maximumCores@ |
--     @minimumCores@ | @maximumSockets@ | @minimumSockets@ |
--     @maximumVcpus@ | @minimumVcpus@
--
-- -   @Sockets@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
--     @maximumSockets@ | @minimumSockets@
--
-- -   @vCPUs@ dimension: @allowedTenancy@ | @honorVcpuOptimization@ |
--     @maximumVcpus@ | @minimumVcpus@
--
-- The unit for @licenseAffinityToHost@ is days and the range is 1 to 180.
-- The possible values for @allowedTenancy@ are @EC2-Default@,
-- @EC2-DedicatedHost@, and @EC2-DedicatedInstance@. The possible values
-- for @honorVcpuOptimization@ are @True@ and @False@.
--
-- 'productInformationList', 'createLicenseConfiguration_productInformationList' - Product information.
--
-- 'tags', 'createLicenseConfiguration_tags' - Tags to add to the license configuration.
--
-- 'name', 'createLicenseConfiguration_name' - Name of the license configuration.
--
-- 'licenseCountingType', 'createLicenseConfiguration_licenseCountingType' - Dimension used to track the license inventory.
newCreateLicenseConfiguration ::
  -- | 'name'
  Prelude.Text ->
  -- | 'licenseCountingType'
  LicenseCountingType ->
  CreateLicenseConfiguration
newCreateLicenseConfiguration
  pName_
  pLicenseCountingType_ =
    CreateLicenseConfiguration'
      { description =
          Prelude.Nothing,
        disassociateWhenNotFound = Prelude.Nothing,
        licenseCount = Prelude.Nothing,
        licenseCountHardLimit = Prelude.Nothing,
        licenseRules = Prelude.Nothing,
        productInformationList = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        licenseCountingType = pLicenseCountingType_
      }

-- | Description of the license configuration.
createLicenseConfiguration_description :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Text)
createLicenseConfiguration_description = Lens.lens (\CreateLicenseConfiguration' {description} -> description) (\s@CreateLicenseConfiguration' {} a -> s {description = a} :: CreateLicenseConfiguration)

-- | When true, disassociates a resource when software is uninstalled.
createLicenseConfiguration_disassociateWhenNotFound :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Bool)
createLicenseConfiguration_disassociateWhenNotFound = Lens.lens (\CreateLicenseConfiguration' {disassociateWhenNotFound} -> disassociateWhenNotFound) (\s@CreateLicenseConfiguration' {} a -> s {disassociateWhenNotFound = a} :: CreateLicenseConfiguration)

-- | Number of licenses managed by the license configuration.
createLicenseConfiguration_licenseCount :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Integer)
createLicenseConfiguration_licenseCount = Lens.lens (\CreateLicenseConfiguration' {licenseCount} -> licenseCount) (\s@CreateLicenseConfiguration' {} a -> s {licenseCount = a} :: CreateLicenseConfiguration)

-- | Indicates whether hard or soft license enforcement is used. Exceeding a
-- hard limit blocks the launch of new instances.
createLicenseConfiguration_licenseCountHardLimit :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Bool)
createLicenseConfiguration_licenseCountHardLimit = Lens.lens (\CreateLicenseConfiguration' {licenseCountHardLimit} -> licenseCountHardLimit) (\s@CreateLicenseConfiguration' {} a -> s {licenseCountHardLimit = a} :: CreateLicenseConfiguration)

-- | License rules. The syntax is #name=value (for example,
-- #allowedTenancy=EC2-DedicatedHost). The available rules vary by
-- dimension, as follows.
--
-- -   @Cores@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
--     @maximumCores@ | @minimumCores@
--
-- -   @Instances@ dimension: @allowedTenancy@ | @maximumCores@ |
--     @minimumCores@ | @maximumSockets@ | @minimumSockets@ |
--     @maximumVcpus@ | @minimumVcpus@
--
-- -   @Sockets@ dimension: @allowedTenancy@ | @licenseAffinityToHost@ |
--     @maximumSockets@ | @minimumSockets@
--
-- -   @vCPUs@ dimension: @allowedTenancy@ | @honorVcpuOptimization@ |
--     @maximumVcpus@ | @minimumVcpus@
--
-- The unit for @licenseAffinityToHost@ is days and the range is 1 to 180.
-- The possible values for @allowedTenancy@ are @EC2-Default@,
-- @EC2-DedicatedHost@, and @EC2-DedicatedInstance@. The possible values
-- for @honorVcpuOptimization@ are @True@ and @False@.
createLicenseConfiguration_licenseRules :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe [Prelude.Text])
createLicenseConfiguration_licenseRules = Lens.lens (\CreateLicenseConfiguration' {licenseRules} -> licenseRules) (\s@CreateLicenseConfiguration' {} a -> s {licenseRules = a} :: CreateLicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Product information.
createLicenseConfiguration_productInformationList :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe [ProductInformation])
createLicenseConfiguration_productInformationList = Lens.lens (\CreateLicenseConfiguration' {productInformationList} -> productInformationList) (\s@CreateLicenseConfiguration' {} a -> s {productInformationList = a} :: CreateLicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Tags to add to the license configuration.
createLicenseConfiguration_tags :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe [Tag])
createLicenseConfiguration_tags = Lens.lens (\CreateLicenseConfiguration' {tags} -> tags) (\s@CreateLicenseConfiguration' {} a -> s {tags = a} :: CreateLicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Name of the license configuration.
createLicenseConfiguration_name :: Lens.Lens' CreateLicenseConfiguration Prelude.Text
createLicenseConfiguration_name = Lens.lens (\CreateLicenseConfiguration' {name} -> name) (\s@CreateLicenseConfiguration' {} a -> s {name = a} :: CreateLicenseConfiguration)

-- | Dimension used to track the license inventory.
createLicenseConfiguration_licenseCountingType :: Lens.Lens' CreateLicenseConfiguration LicenseCountingType
createLicenseConfiguration_licenseCountingType = Lens.lens (\CreateLicenseConfiguration' {licenseCountingType} -> licenseCountingType) (\s@CreateLicenseConfiguration' {} a -> s {licenseCountingType = a} :: CreateLicenseConfiguration)

instance Core.AWSRequest CreateLicenseConfiguration where
  type
    AWSResponse CreateLicenseConfiguration =
      CreateLicenseConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLicenseConfigurationResponse'
            Prelude.<$> (x Data..?> "LicenseConfigurationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLicenseConfiguration where
  hashWithSalt _salt CreateLicenseConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disassociateWhenNotFound
      `Prelude.hashWithSalt` licenseCount
      `Prelude.hashWithSalt` licenseCountHardLimit
      `Prelude.hashWithSalt` licenseRules
      `Prelude.hashWithSalt` productInformationList
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` licenseCountingType

instance Prelude.NFData CreateLicenseConfiguration where
  rnf CreateLicenseConfiguration' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf disassociateWhenNotFound
      `Prelude.seq` Prelude.rnf licenseCount
      `Prelude.seq` Prelude.rnf licenseCountHardLimit
      `Prelude.seq` Prelude.rnf licenseRules
      `Prelude.seq` Prelude.rnf productInformationList
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf licenseCountingType

instance Data.ToHeaders CreateLicenseConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLicenseManager.CreateLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLicenseConfiguration where
  toJSON CreateLicenseConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DisassociateWhenNotFound" Data..=)
              Prelude.<$> disassociateWhenNotFound,
            ("LicenseCount" Data..=) Prelude.<$> licenseCount,
            ("LicenseCountHardLimit" Data..=)
              Prelude.<$> licenseCountHardLimit,
            ("LicenseRules" Data..=) Prelude.<$> licenseRules,
            ("ProductInformationList" Data..=)
              Prelude.<$> productInformationList,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("LicenseCountingType" Data..= licenseCountingType)
          ]
      )

instance Data.ToPath CreateLicenseConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLicenseConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLicenseConfigurationResponse' smart constructor.
data CreateLicenseConfigurationResponse = CreateLicenseConfigurationResponse'
  { -- | Amazon Resource Name (ARN) of the license configuration.
    licenseConfigurationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'licenseConfigurationArn', 'createLicenseConfigurationResponse_licenseConfigurationArn' - Amazon Resource Name (ARN) of the license configuration.
--
-- 'httpStatus', 'createLicenseConfigurationResponse_httpStatus' - The response's http status code.
newCreateLicenseConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLicenseConfigurationResponse
newCreateLicenseConfigurationResponse pHttpStatus_ =
  CreateLicenseConfigurationResponse'
    { licenseConfigurationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Amazon Resource Name (ARN) of the license configuration.
createLicenseConfigurationResponse_licenseConfigurationArn :: Lens.Lens' CreateLicenseConfigurationResponse (Prelude.Maybe Prelude.Text)
createLicenseConfigurationResponse_licenseConfigurationArn = Lens.lens (\CreateLicenseConfigurationResponse' {licenseConfigurationArn} -> licenseConfigurationArn) (\s@CreateLicenseConfigurationResponse' {} a -> s {licenseConfigurationArn = a} :: CreateLicenseConfigurationResponse)

-- | The response's http status code.
createLicenseConfigurationResponse_httpStatus :: Lens.Lens' CreateLicenseConfigurationResponse Prelude.Int
createLicenseConfigurationResponse_httpStatus = Lens.lens (\CreateLicenseConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateLicenseConfigurationResponse' {} a -> s {httpStatus = a} :: CreateLicenseConfigurationResponse)

instance
  Prelude.NFData
    CreateLicenseConfigurationResponse
  where
  rnf CreateLicenseConfigurationResponse' {..} =
    Prelude.rnf licenseConfigurationArn
      `Prelude.seq` Prelude.rnf httpStatus
