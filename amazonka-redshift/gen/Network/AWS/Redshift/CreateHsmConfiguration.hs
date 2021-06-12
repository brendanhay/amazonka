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
-- Module      : Network.AWS.Redshift.CreateHsmConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an HSM configuration that contains the information required by
-- an Amazon Redshift cluster to store and use database encryption keys in
-- a Hardware Security Module (HSM). After creating the HSM configuration,
-- you can specify it as a parameter when creating a cluster. The cluster
-- will then store its encryption keys in the HSM.
--
-- In addition to creating an HSM configuration, you must also create an
-- HSM client certificate. For more information, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-HSM.html Hardware Security Modules>
-- in the Amazon Redshift Cluster Management Guide.
module Network.AWS.Redshift.CreateHsmConfiguration
  ( -- * Creating a Request
    CreateHsmConfiguration (..),
    newCreateHsmConfiguration,

    -- * Request Lenses
    createHsmConfiguration_tags,
    createHsmConfiguration_hsmConfigurationIdentifier,
    createHsmConfiguration_description,
    createHsmConfiguration_hsmIpAddress,
    createHsmConfiguration_hsmPartitionName,
    createHsmConfiguration_hsmPartitionPassword,
    createHsmConfiguration_hsmServerPublicCertificate,

    -- * Destructuring the Response
    CreateHsmConfigurationResponse (..),
    newCreateHsmConfigurationResponse,

    -- * Response Lenses
    createHsmConfigurationResponse_hsmConfiguration,
    createHsmConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newCreateHsmConfiguration' smart constructor.
data CreateHsmConfiguration = CreateHsmConfiguration'
  { -- | A list of tag instances.
    tags :: Core.Maybe [Tag],
    -- | The identifier to be assigned to the new Amazon Redshift HSM
    -- configuration.
    hsmConfigurationIdentifier :: Core.Text,
    -- | A text description of the HSM configuration to be created.
    description :: Core.Text,
    -- | The IP address that the Amazon Redshift cluster must use to access the
    -- HSM.
    hsmIpAddress :: Core.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters
    -- will store their database encryption keys.
    hsmPartitionName :: Core.Text,
    -- | The password required to access the HSM partition.
    hsmPartitionPassword :: Core.Text,
    -- | The HSMs public certificate file. When using Cloud HSM, the file name is
    -- server.pem.
    hsmServerPublicCertificate :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsmConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createHsmConfiguration_tags' - A list of tag instances.
--
-- 'hsmConfigurationIdentifier', 'createHsmConfiguration_hsmConfigurationIdentifier' - The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
--
-- 'description', 'createHsmConfiguration_description' - A text description of the HSM configuration to be created.
--
-- 'hsmIpAddress', 'createHsmConfiguration_hsmIpAddress' - The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
--
-- 'hsmPartitionName', 'createHsmConfiguration_hsmPartitionName' - The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
--
-- 'hsmPartitionPassword', 'createHsmConfiguration_hsmPartitionPassword' - The password required to access the HSM partition.
--
-- 'hsmServerPublicCertificate', 'createHsmConfiguration_hsmServerPublicCertificate' - The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
newCreateHsmConfiguration ::
  -- | 'hsmConfigurationIdentifier'
  Core.Text ->
  -- | 'description'
  Core.Text ->
  -- | 'hsmIpAddress'
  Core.Text ->
  -- | 'hsmPartitionName'
  Core.Text ->
  -- | 'hsmPartitionPassword'
  Core.Text ->
  -- | 'hsmServerPublicCertificate'
  Core.Text ->
  CreateHsmConfiguration
newCreateHsmConfiguration
  pHsmConfigurationIdentifier_
  pDescription_
  pHsmIpAddress_
  pHsmPartitionName_
  pHsmPartitionPassword_
  pHsmServerPublicCertificate_ =
    CreateHsmConfiguration'
      { tags = Core.Nothing,
        hsmConfigurationIdentifier =
          pHsmConfigurationIdentifier_,
        description = pDescription_,
        hsmIpAddress = pHsmIpAddress_,
        hsmPartitionName = pHsmPartitionName_,
        hsmPartitionPassword = pHsmPartitionPassword_,
        hsmServerPublicCertificate =
          pHsmServerPublicCertificate_
      }

-- | A list of tag instances.
createHsmConfiguration_tags :: Lens.Lens' CreateHsmConfiguration (Core.Maybe [Tag])
createHsmConfiguration_tags = Lens.lens (\CreateHsmConfiguration' {tags} -> tags) (\s@CreateHsmConfiguration' {} a -> s {tags = a} :: CreateHsmConfiguration) Core.. Lens.mapping Lens._Coerce

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
createHsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\CreateHsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@CreateHsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: CreateHsmConfiguration)

-- | A text description of the HSM configuration to be created.
createHsmConfiguration_description :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_description = Lens.lens (\CreateHsmConfiguration' {description} -> description) (\s@CreateHsmConfiguration' {} a -> s {description = a} :: CreateHsmConfiguration)

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
createHsmConfiguration_hsmIpAddress :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_hsmIpAddress = Lens.lens (\CreateHsmConfiguration' {hsmIpAddress} -> hsmIpAddress) (\s@CreateHsmConfiguration' {} a -> s {hsmIpAddress = a} :: CreateHsmConfiguration)

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
createHsmConfiguration_hsmPartitionName :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_hsmPartitionName = Lens.lens (\CreateHsmConfiguration' {hsmPartitionName} -> hsmPartitionName) (\s@CreateHsmConfiguration' {} a -> s {hsmPartitionName = a} :: CreateHsmConfiguration)

-- | The password required to access the HSM partition.
createHsmConfiguration_hsmPartitionPassword :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_hsmPartitionPassword = Lens.lens (\CreateHsmConfiguration' {hsmPartitionPassword} -> hsmPartitionPassword) (\s@CreateHsmConfiguration' {} a -> s {hsmPartitionPassword = a} :: CreateHsmConfiguration)

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
createHsmConfiguration_hsmServerPublicCertificate :: Lens.Lens' CreateHsmConfiguration Core.Text
createHsmConfiguration_hsmServerPublicCertificate = Lens.lens (\CreateHsmConfiguration' {hsmServerPublicCertificate} -> hsmServerPublicCertificate) (\s@CreateHsmConfiguration' {} a -> s {hsmServerPublicCertificate = a} :: CreateHsmConfiguration)

instance Core.AWSRequest CreateHsmConfiguration where
  type
    AWSResponse CreateHsmConfiguration =
      CreateHsmConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateHsmConfigurationResult"
      ( \s h x ->
          CreateHsmConfigurationResponse'
            Core.<$> (x Core..@? "HsmConfiguration")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateHsmConfiguration

instance Core.NFData CreateHsmConfiguration

instance Core.ToHeaders CreateHsmConfiguration where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateHsmConfiguration where
  toPath = Core.const "/"

instance Core.ToQuery CreateHsmConfiguration where
  toQuery CreateHsmConfiguration' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateHsmConfiguration" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "Tags"
          Core.=: Core.toQuery (Core.toQueryList "Tag" Core.<$> tags),
        "HsmConfigurationIdentifier"
          Core.=: hsmConfigurationIdentifier,
        "Description" Core.=: description,
        "HsmIpAddress" Core.=: hsmIpAddress,
        "HsmPartitionName" Core.=: hsmPartitionName,
        "HsmPartitionPassword" Core.=: hsmPartitionPassword,
        "HsmServerPublicCertificate"
          Core.=: hsmServerPublicCertificate
      ]

-- | /See:/ 'newCreateHsmConfigurationResponse' smart constructor.
data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse'
  { hsmConfiguration :: Core.Maybe HsmConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateHsmConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hsmConfiguration', 'createHsmConfigurationResponse_hsmConfiguration' - Undocumented member.
--
-- 'httpStatus', 'createHsmConfigurationResponse_httpStatus' - The response's http status code.
newCreateHsmConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateHsmConfigurationResponse
newCreateHsmConfigurationResponse pHttpStatus_ =
  CreateHsmConfigurationResponse'
    { hsmConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createHsmConfigurationResponse_hsmConfiguration :: Lens.Lens' CreateHsmConfigurationResponse (Core.Maybe HsmConfiguration)
createHsmConfigurationResponse_hsmConfiguration = Lens.lens (\CreateHsmConfigurationResponse' {hsmConfiguration} -> hsmConfiguration) (\s@CreateHsmConfigurationResponse' {} a -> s {hsmConfiguration = a} :: CreateHsmConfigurationResponse)

-- | The response's http status code.
createHsmConfigurationResponse_httpStatus :: Lens.Lens' CreateHsmConfigurationResponse Core.Int
createHsmConfigurationResponse_httpStatus = Lens.lens (\CreateHsmConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateHsmConfigurationResponse' {} a -> s {httpStatus = a} :: CreateHsmConfigurationResponse)

instance Core.NFData CreateHsmConfigurationResponse
