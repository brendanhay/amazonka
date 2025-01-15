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
-- Module      : Amazonka.Redshift.CreateHsmConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.Redshift.CreateHsmConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newCreateHsmConfiguration' smart constructor.
data CreateHsmConfiguration = CreateHsmConfiguration'
  { -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag],
    -- | The identifier to be assigned to the new Amazon Redshift HSM
    -- configuration.
    hsmConfigurationIdentifier :: Prelude.Text,
    -- | A text description of the HSM configuration to be created.
    description :: Prelude.Text,
    -- | The IP address that the Amazon Redshift cluster must use to access the
    -- HSM.
    hsmIpAddress :: Prelude.Text,
    -- | The name of the partition in the HSM where the Amazon Redshift clusters
    -- will store their database encryption keys.
    hsmPartitionName :: Prelude.Text,
    -- | The password required to access the HSM partition.
    hsmPartitionPassword :: Prelude.Text,
    -- | The HSMs public certificate file. When using Cloud HSM, the file name is
    -- server.pem.
    hsmServerPublicCertificate :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'hsmIpAddress'
  Prelude.Text ->
  -- | 'hsmPartitionName'
  Prelude.Text ->
  -- | 'hsmPartitionPassword'
  Prelude.Text ->
  -- | 'hsmServerPublicCertificate'
  Prelude.Text ->
  CreateHsmConfiguration
newCreateHsmConfiguration
  pHsmConfigurationIdentifier_
  pDescription_
  pHsmIpAddress_
  pHsmPartitionName_
  pHsmPartitionPassword_
  pHsmServerPublicCertificate_ =
    CreateHsmConfiguration'
      { tags = Prelude.Nothing,
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
createHsmConfiguration_tags :: Lens.Lens' CreateHsmConfiguration (Prelude.Maybe [Tag])
createHsmConfiguration_tags = Lens.lens (\CreateHsmConfiguration' {tags} -> tags) (\s@CreateHsmConfiguration' {} a -> s {tags = a} :: CreateHsmConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The identifier to be assigned to the new Amazon Redshift HSM
-- configuration.
createHsmConfiguration_hsmConfigurationIdentifier :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_hsmConfigurationIdentifier = Lens.lens (\CreateHsmConfiguration' {hsmConfigurationIdentifier} -> hsmConfigurationIdentifier) (\s@CreateHsmConfiguration' {} a -> s {hsmConfigurationIdentifier = a} :: CreateHsmConfiguration)

-- | A text description of the HSM configuration to be created.
createHsmConfiguration_description :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_description = Lens.lens (\CreateHsmConfiguration' {description} -> description) (\s@CreateHsmConfiguration' {} a -> s {description = a} :: CreateHsmConfiguration)

-- | The IP address that the Amazon Redshift cluster must use to access the
-- HSM.
createHsmConfiguration_hsmIpAddress :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_hsmIpAddress = Lens.lens (\CreateHsmConfiguration' {hsmIpAddress} -> hsmIpAddress) (\s@CreateHsmConfiguration' {} a -> s {hsmIpAddress = a} :: CreateHsmConfiguration)

-- | The name of the partition in the HSM where the Amazon Redshift clusters
-- will store their database encryption keys.
createHsmConfiguration_hsmPartitionName :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_hsmPartitionName = Lens.lens (\CreateHsmConfiguration' {hsmPartitionName} -> hsmPartitionName) (\s@CreateHsmConfiguration' {} a -> s {hsmPartitionName = a} :: CreateHsmConfiguration)

-- | The password required to access the HSM partition.
createHsmConfiguration_hsmPartitionPassword :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_hsmPartitionPassword = Lens.lens (\CreateHsmConfiguration' {hsmPartitionPassword} -> hsmPartitionPassword) (\s@CreateHsmConfiguration' {} a -> s {hsmPartitionPassword = a} :: CreateHsmConfiguration)

-- | The HSMs public certificate file. When using Cloud HSM, the file name is
-- server.pem.
createHsmConfiguration_hsmServerPublicCertificate :: Lens.Lens' CreateHsmConfiguration Prelude.Text
createHsmConfiguration_hsmServerPublicCertificate = Lens.lens (\CreateHsmConfiguration' {hsmServerPublicCertificate} -> hsmServerPublicCertificate) (\s@CreateHsmConfiguration' {} a -> s {hsmServerPublicCertificate = a} :: CreateHsmConfiguration)

instance Core.AWSRequest CreateHsmConfiguration where
  type
    AWSResponse CreateHsmConfiguration =
      CreateHsmConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateHsmConfigurationResult"
      ( \s h x ->
          CreateHsmConfigurationResponse'
            Prelude.<$> (x Data..@? "HsmConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateHsmConfiguration where
  hashWithSalt _salt CreateHsmConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` hsmConfigurationIdentifier
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` hsmIpAddress
      `Prelude.hashWithSalt` hsmPartitionName
      `Prelude.hashWithSalt` hsmPartitionPassword
      `Prelude.hashWithSalt` hsmServerPublicCertificate

instance Prelude.NFData CreateHsmConfiguration where
  rnf CreateHsmConfiguration' {..} =
    Prelude.rnf tags `Prelude.seq`
      Prelude.rnf hsmConfigurationIdentifier `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf hsmIpAddress `Prelude.seq`
            Prelude.rnf hsmPartitionName `Prelude.seq`
              Prelude.rnf hsmPartitionPassword `Prelude.seq`
                Prelude.rnf hsmServerPublicCertificate

instance Data.ToHeaders CreateHsmConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateHsmConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateHsmConfiguration where
  toQuery CreateHsmConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateHsmConfiguration" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "Tag" Prelude.<$> tags),
        "HsmConfigurationIdentifier"
          Data.=: hsmConfigurationIdentifier,
        "Description" Data.=: description,
        "HsmIpAddress" Data.=: hsmIpAddress,
        "HsmPartitionName" Data.=: hsmPartitionName,
        "HsmPartitionPassword" Data.=: hsmPartitionPassword,
        "HsmServerPublicCertificate"
          Data.=: hsmServerPublicCertificate
      ]

-- | /See:/ 'newCreateHsmConfigurationResponse' smart constructor.
data CreateHsmConfigurationResponse = CreateHsmConfigurationResponse'
  { hsmConfiguration :: Prelude.Maybe HsmConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CreateHsmConfigurationResponse
newCreateHsmConfigurationResponse pHttpStatus_ =
  CreateHsmConfigurationResponse'
    { hsmConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
createHsmConfigurationResponse_hsmConfiguration :: Lens.Lens' CreateHsmConfigurationResponse (Prelude.Maybe HsmConfiguration)
createHsmConfigurationResponse_hsmConfiguration = Lens.lens (\CreateHsmConfigurationResponse' {hsmConfiguration} -> hsmConfiguration) (\s@CreateHsmConfigurationResponse' {} a -> s {hsmConfiguration = a} :: CreateHsmConfigurationResponse)

-- | The response's http status code.
createHsmConfigurationResponse_httpStatus :: Lens.Lens' CreateHsmConfigurationResponse Prelude.Int
createHsmConfigurationResponse_httpStatus = Lens.lens (\CreateHsmConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateHsmConfigurationResponse' {} a -> s {httpStatus = a} :: CreateHsmConfigurationResponse)

instance
  Prelude.NFData
    CreateHsmConfigurationResponse
  where
  rnf CreateHsmConfigurationResponse' {..} =
    Prelude.rnf hsmConfiguration `Prelude.seq`
      Prelude.rnf httpStatus
