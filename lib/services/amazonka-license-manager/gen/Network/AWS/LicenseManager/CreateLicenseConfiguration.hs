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
-- Module      : Network.AWS.LicenseManager.CreateLicenseConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.LicenseManager.CreateLicenseConfiguration
  ( -- * Creating a Request
    CreateLicenseConfiguration (..),
    newCreateLicenseConfiguration,

    -- * Request Lenses
    createLicenseConfiguration_licenseCount,
    createLicenseConfiguration_licenseCountHardLimit,
    createLicenseConfiguration_disassociateWhenNotFound,
    createLicenseConfiguration_productInformationList,
    createLicenseConfiguration_licenseRules,
    createLicenseConfiguration_description,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.LicenseManager.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateLicenseConfiguration' smart constructor.
data CreateLicenseConfiguration = CreateLicenseConfiguration'
  { -- | Number of licenses managed by the license configuration.
    licenseCount :: Prelude.Maybe Prelude.Integer,
    -- | Indicates whether hard or soft license enforcement is used. Exceeding a
    -- hard limit blocks the launch of new instances.
    licenseCountHardLimit :: Prelude.Maybe Prelude.Bool,
    -- | When true, disassociates a resource when software is uninstalled.
    disassociateWhenNotFound :: Prelude.Maybe Prelude.Bool,
    -- | Product information.
    productInformationList :: Prelude.Maybe [ProductInformation],
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
    -- | Description of the license configuration.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'licenseCount', 'createLicenseConfiguration_licenseCount' - Number of licenses managed by the license configuration.
--
-- 'licenseCountHardLimit', 'createLicenseConfiguration_licenseCountHardLimit' - Indicates whether hard or soft license enforcement is used. Exceeding a
-- hard limit blocks the launch of new instances.
--
-- 'disassociateWhenNotFound', 'createLicenseConfiguration_disassociateWhenNotFound' - When true, disassociates a resource when software is uninstalled.
--
-- 'productInformationList', 'createLicenseConfiguration_productInformationList' - Product information.
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
-- 'description', 'createLicenseConfiguration_description' - Description of the license configuration.
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
      { licenseCount =
          Prelude.Nothing,
        licenseCountHardLimit = Prelude.Nothing,
        disassociateWhenNotFound = Prelude.Nothing,
        productInformationList = Prelude.Nothing,
        licenseRules = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        licenseCountingType = pLicenseCountingType_
      }

-- | Number of licenses managed by the license configuration.
createLicenseConfiguration_licenseCount :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Integer)
createLicenseConfiguration_licenseCount = Lens.lens (\CreateLicenseConfiguration' {licenseCount} -> licenseCount) (\s@CreateLicenseConfiguration' {} a -> s {licenseCount = a} :: CreateLicenseConfiguration)

-- | Indicates whether hard or soft license enforcement is used. Exceeding a
-- hard limit blocks the launch of new instances.
createLicenseConfiguration_licenseCountHardLimit :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Bool)
createLicenseConfiguration_licenseCountHardLimit = Lens.lens (\CreateLicenseConfiguration' {licenseCountHardLimit} -> licenseCountHardLimit) (\s@CreateLicenseConfiguration' {} a -> s {licenseCountHardLimit = a} :: CreateLicenseConfiguration)

-- | When true, disassociates a resource when software is uninstalled.
createLicenseConfiguration_disassociateWhenNotFound :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Bool)
createLicenseConfiguration_disassociateWhenNotFound = Lens.lens (\CreateLicenseConfiguration' {disassociateWhenNotFound} -> disassociateWhenNotFound) (\s@CreateLicenseConfiguration' {} a -> s {disassociateWhenNotFound = a} :: CreateLicenseConfiguration)

-- | Product information.
createLicenseConfiguration_productInformationList :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe [ProductInformation])
createLicenseConfiguration_productInformationList = Lens.lens (\CreateLicenseConfiguration' {productInformationList} -> productInformationList) (\s@CreateLicenseConfiguration' {} a -> s {productInformationList = a} :: CreateLicenseConfiguration) Prelude.. Lens.mapping Lens.coerced

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

-- | Description of the license configuration.
createLicenseConfiguration_description :: Lens.Lens' CreateLicenseConfiguration (Prelude.Maybe Prelude.Text)
createLicenseConfiguration_description = Lens.lens (\CreateLicenseConfiguration' {description} -> description) (\s@CreateLicenseConfiguration' {} a -> s {description = a} :: CreateLicenseConfiguration)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLicenseConfigurationResponse'
            Prelude.<$> (x Core..?> "LicenseConfigurationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLicenseConfiguration

instance Prelude.NFData CreateLicenseConfiguration

instance Core.ToHeaders CreateLicenseConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.CreateLicenseConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLicenseConfiguration where
  toJSON CreateLicenseConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LicenseCount" Core..=) Prelude.<$> licenseCount,
            ("LicenseCountHardLimit" Core..=)
              Prelude.<$> licenseCountHardLimit,
            ("DisassociateWhenNotFound" Core..=)
              Prelude.<$> disassociateWhenNotFound,
            ("ProductInformationList" Core..=)
              Prelude.<$> productInformationList,
            ("LicenseRules" Core..=) Prelude.<$> licenseRules,
            ("Description" Core..=) Prelude.<$> description,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("LicenseCountingType" Core..= licenseCountingType)
          ]
      )

instance Core.ToPath CreateLicenseConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLicenseConfiguration where
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
