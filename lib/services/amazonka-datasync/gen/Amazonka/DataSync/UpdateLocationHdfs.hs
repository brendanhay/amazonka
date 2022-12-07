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
-- Module      : Amazonka.DataSync.UpdateLocationHdfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some parameters of a previously created location for a Hadoop
-- Distributed File System cluster.
module Amazonka.DataSync.UpdateLocationHdfs
  ( -- * Creating a Request
    UpdateLocationHdfs (..),
    newUpdateLocationHdfs,

    -- * Request Lenses
    updateLocationHdfs_authenticationType,
    updateLocationHdfs_kerberosKrb5Conf,
    updateLocationHdfs_kerberosPrincipal,
    updateLocationHdfs_replicationFactor,
    updateLocationHdfs_kmsKeyProviderUri,
    updateLocationHdfs_qopConfiguration,
    updateLocationHdfs_nameNodes,
    updateLocationHdfs_simpleUser,
    updateLocationHdfs_blockSize,
    updateLocationHdfs_kerberosKeytab,
    updateLocationHdfs_subdirectory,
    updateLocationHdfs_agentArns,
    updateLocationHdfs_locationArn,

    -- * Destructuring the Response
    UpdateLocationHdfsResponse (..),
    newUpdateLocationHdfsResponse,

    -- * Response Lenses
    updateLocationHdfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLocationHdfs' smart constructor.
data UpdateLocationHdfs = UpdateLocationHdfs'
  { -- | The type of authentication used to determine the identity of the user.
    authenticationType :: Prelude.Maybe HdfsAuthenticationType,
    -- | The @krb5.conf@ file that contains the Kerberos configuration
    -- information. You can load the @krb5.conf@ file by providing the file\'s
    -- address. If you\'re using the CLI, it performs the base64 encoding for
    -- you. Otherwise, provide the base64-encoded text.
    kerberosKrb5Conf :: Prelude.Maybe Data.Base64,
    -- | The Kerberos principal with access to the files and folders on the HDFS
    -- cluster.
    kerberosPrincipal :: Prelude.Maybe Prelude.Text,
    -- | The number of DataNodes to replicate the data to when writing to the
    -- HDFS cluster.
    replicationFactor :: Prelude.Maybe Prelude.Natural,
    -- | The URI of the HDFS cluster\'s Key Management Server (KMS).
    kmsKeyProviderUri :: Prelude.Maybe Prelude.Text,
    -- | The Quality of Protection (QOP) configuration specifies the Remote
    -- Procedure Call (RPC) and data transfer privacy settings configured on
    -- the Hadoop Distributed File System (HDFS) cluster.
    qopConfiguration :: Prelude.Maybe QopConfiguration,
    -- | The NameNode that manages the HDFS namespace. The NameNode performs
    -- operations such as opening, closing, and renaming files and directories.
    -- The NameNode contains the information to map blocks of data to the
    -- DataNodes. You can use only one NameNode.
    nameNodes :: Prelude.Maybe (Prelude.NonEmpty HdfsNameNode),
    -- | The user name used to identify the client on the host operating system.
    simpleUser :: Prelude.Maybe Prelude.Text,
    -- | The size of the data blocks to write into the HDFS cluster.
    blockSize :: Prelude.Maybe Prelude.Natural,
    -- | The Kerberos key table (keytab) that contains mappings between the
    -- defined Kerberos principal and the encrypted keys. You can load the
    -- keytab from a file by providing the file\'s address. If you use the CLI,
    -- it performs base64 encoding for you. Otherwise, provide the
    -- base64-encoded text.
    kerberosKeytab :: Prelude.Maybe Data.Base64,
    -- | A subdirectory in the HDFS cluster. This subdirectory is used to read
    -- data from or write data to the HDFS cluster.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The ARNs of the agents that are used to connect to the HDFS cluster.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the source HDFS cluster location.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationHdfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authenticationType', 'updateLocationHdfs_authenticationType' - The type of authentication used to determine the identity of the user.
--
-- 'kerberosKrb5Conf', 'updateLocationHdfs_kerberosKrb5Conf' - The @krb5.conf@ file that contains the Kerberos configuration
-- information. You can load the @krb5.conf@ file by providing the file\'s
-- address. If you\'re using the CLI, it performs the base64 encoding for
-- you. Otherwise, provide the base64-encoded text.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'kerberosPrincipal', 'updateLocationHdfs_kerberosPrincipal' - The Kerberos principal with access to the files and folders on the HDFS
-- cluster.
--
-- 'replicationFactor', 'updateLocationHdfs_replicationFactor' - The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster.
--
-- 'kmsKeyProviderUri', 'updateLocationHdfs_kmsKeyProviderUri' - The URI of the HDFS cluster\'s Key Management Server (KMS).
--
-- 'qopConfiguration', 'updateLocationHdfs_qopConfiguration' - The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer privacy settings configured on
-- the Hadoop Distributed File System (HDFS) cluster.
--
-- 'nameNodes', 'updateLocationHdfs_nameNodes' - The NameNode that manages the HDFS namespace. The NameNode performs
-- operations such as opening, closing, and renaming files and directories.
-- The NameNode contains the information to map blocks of data to the
-- DataNodes. You can use only one NameNode.
--
-- 'simpleUser', 'updateLocationHdfs_simpleUser' - The user name used to identify the client on the host operating system.
--
-- 'blockSize', 'updateLocationHdfs_blockSize' - The size of the data blocks to write into the HDFS cluster.
--
-- 'kerberosKeytab', 'updateLocationHdfs_kerberosKeytab' - The Kerberos key table (keytab) that contains mappings between the
-- defined Kerberos principal and the encrypted keys. You can load the
-- keytab from a file by providing the file\'s address. If you use the CLI,
-- it performs base64 encoding for you. Otherwise, provide the
-- base64-encoded text.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'subdirectory', 'updateLocationHdfs_subdirectory' - A subdirectory in the HDFS cluster. This subdirectory is used to read
-- data from or write data to the HDFS cluster.
--
-- 'agentArns', 'updateLocationHdfs_agentArns' - The ARNs of the agents that are used to connect to the HDFS cluster.
--
-- 'locationArn', 'updateLocationHdfs_locationArn' - The Amazon Resource Name (ARN) of the source HDFS cluster location.
newUpdateLocationHdfs ::
  -- | 'locationArn'
  Prelude.Text ->
  UpdateLocationHdfs
newUpdateLocationHdfs pLocationArn_ =
  UpdateLocationHdfs'
    { authenticationType =
        Prelude.Nothing,
      kerberosKrb5Conf = Prelude.Nothing,
      kerberosPrincipal = Prelude.Nothing,
      replicationFactor = Prelude.Nothing,
      kmsKeyProviderUri = Prelude.Nothing,
      qopConfiguration = Prelude.Nothing,
      nameNodes = Prelude.Nothing,
      simpleUser = Prelude.Nothing,
      blockSize = Prelude.Nothing,
      kerberosKeytab = Prelude.Nothing,
      subdirectory = Prelude.Nothing,
      agentArns = Prelude.Nothing,
      locationArn = pLocationArn_
    }

-- | The type of authentication used to determine the identity of the user.
updateLocationHdfs_authenticationType :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe HdfsAuthenticationType)
updateLocationHdfs_authenticationType = Lens.lens (\UpdateLocationHdfs' {authenticationType} -> authenticationType) (\s@UpdateLocationHdfs' {} a -> s {authenticationType = a} :: UpdateLocationHdfs)

-- | The @krb5.conf@ file that contains the Kerberos configuration
-- information. You can load the @krb5.conf@ file by providing the file\'s
-- address. If you\'re using the CLI, it performs the base64 encoding for
-- you. Otherwise, provide the base64-encoded text.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateLocationHdfs_kerberosKrb5Conf :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.ByteString)
updateLocationHdfs_kerberosKrb5Conf = Lens.lens (\UpdateLocationHdfs' {kerberosKrb5Conf} -> kerberosKrb5Conf) (\s@UpdateLocationHdfs' {} a -> s {kerberosKrb5Conf = a} :: UpdateLocationHdfs) Prelude.. Lens.mapping Data._Base64

-- | The Kerberos principal with access to the files and folders on the HDFS
-- cluster.
updateLocationHdfs_kerberosPrincipal :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Text)
updateLocationHdfs_kerberosPrincipal = Lens.lens (\UpdateLocationHdfs' {kerberosPrincipal} -> kerberosPrincipal) (\s@UpdateLocationHdfs' {} a -> s {kerberosPrincipal = a} :: UpdateLocationHdfs)

-- | The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster.
updateLocationHdfs_replicationFactor :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Natural)
updateLocationHdfs_replicationFactor = Lens.lens (\UpdateLocationHdfs' {replicationFactor} -> replicationFactor) (\s@UpdateLocationHdfs' {} a -> s {replicationFactor = a} :: UpdateLocationHdfs)

-- | The URI of the HDFS cluster\'s Key Management Server (KMS).
updateLocationHdfs_kmsKeyProviderUri :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Text)
updateLocationHdfs_kmsKeyProviderUri = Lens.lens (\UpdateLocationHdfs' {kmsKeyProviderUri} -> kmsKeyProviderUri) (\s@UpdateLocationHdfs' {} a -> s {kmsKeyProviderUri = a} :: UpdateLocationHdfs)

-- | The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer privacy settings configured on
-- the Hadoop Distributed File System (HDFS) cluster.
updateLocationHdfs_qopConfiguration :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe QopConfiguration)
updateLocationHdfs_qopConfiguration = Lens.lens (\UpdateLocationHdfs' {qopConfiguration} -> qopConfiguration) (\s@UpdateLocationHdfs' {} a -> s {qopConfiguration = a} :: UpdateLocationHdfs)

-- | The NameNode that manages the HDFS namespace. The NameNode performs
-- operations such as opening, closing, and renaming files and directories.
-- The NameNode contains the information to map blocks of data to the
-- DataNodes. You can use only one NameNode.
updateLocationHdfs_nameNodes :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe (Prelude.NonEmpty HdfsNameNode))
updateLocationHdfs_nameNodes = Lens.lens (\UpdateLocationHdfs' {nameNodes} -> nameNodes) (\s@UpdateLocationHdfs' {} a -> s {nameNodes = a} :: UpdateLocationHdfs) Prelude.. Lens.mapping Lens.coerced

-- | The user name used to identify the client on the host operating system.
updateLocationHdfs_simpleUser :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Text)
updateLocationHdfs_simpleUser = Lens.lens (\UpdateLocationHdfs' {simpleUser} -> simpleUser) (\s@UpdateLocationHdfs' {} a -> s {simpleUser = a} :: UpdateLocationHdfs)

-- | The size of the data blocks to write into the HDFS cluster.
updateLocationHdfs_blockSize :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Natural)
updateLocationHdfs_blockSize = Lens.lens (\UpdateLocationHdfs' {blockSize} -> blockSize) (\s@UpdateLocationHdfs' {} a -> s {blockSize = a} :: UpdateLocationHdfs)

-- | The Kerberos key table (keytab) that contains mappings between the
-- defined Kerberos principal and the encrypted keys. You can load the
-- keytab from a file by providing the file\'s address. If you use the CLI,
-- it performs base64 encoding for you. Otherwise, provide the
-- base64-encoded text.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateLocationHdfs_kerberosKeytab :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.ByteString)
updateLocationHdfs_kerberosKeytab = Lens.lens (\UpdateLocationHdfs' {kerberosKeytab} -> kerberosKeytab) (\s@UpdateLocationHdfs' {} a -> s {kerberosKeytab = a} :: UpdateLocationHdfs) Prelude.. Lens.mapping Data._Base64

-- | A subdirectory in the HDFS cluster. This subdirectory is used to read
-- data from or write data to the HDFS cluster.
updateLocationHdfs_subdirectory :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe Prelude.Text)
updateLocationHdfs_subdirectory = Lens.lens (\UpdateLocationHdfs' {subdirectory} -> subdirectory) (\s@UpdateLocationHdfs' {} a -> s {subdirectory = a} :: UpdateLocationHdfs)

-- | The ARNs of the agents that are used to connect to the HDFS cluster.
updateLocationHdfs_agentArns :: Lens.Lens' UpdateLocationHdfs (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLocationHdfs_agentArns = Lens.lens (\UpdateLocationHdfs' {agentArns} -> agentArns) (\s@UpdateLocationHdfs' {} a -> s {agentArns = a} :: UpdateLocationHdfs) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the source HDFS cluster location.
updateLocationHdfs_locationArn :: Lens.Lens' UpdateLocationHdfs Prelude.Text
updateLocationHdfs_locationArn = Lens.lens (\UpdateLocationHdfs' {locationArn} -> locationArn) (\s@UpdateLocationHdfs' {} a -> s {locationArn = a} :: UpdateLocationHdfs)

instance Core.AWSRequest UpdateLocationHdfs where
  type
    AWSResponse UpdateLocationHdfs =
      UpdateLocationHdfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLocationHdfsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLocationHdfs where
  hashWithSalt _salt UpdateLocationHdfs' {..} =
    _salt `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` kerberosKrb5Conf
      `Prelude.hashWithSalt` kerberosPrincipal
      `Prelude.hashWithSalt` replicationFactor
      `Prelude.hashWithSalt` kmsKeyProviderUri
      `Prelude.hashWithSalt` qopConfiguration
      `Prelude.hashWithSalt` nameNodes
      `Prelude.hashWithSalt` simpleUser
      `Prelude.hashWithSalt` blockSize
      `Prelude.hashWithSalt` kerberosKeytab
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` locationArn

instance Prelude.NFData UpdateLocationHdfs where
  rnf UpdateLocationHdfs' {..} =
    Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf kerberosKrb5Conf
      `Prelude.seq` Prelude.rnf kerberosPrincipal
      `Prelude.seq` Prelude.rnf replicationFactor
      `Prelude.seq` Prelude.rnf kmsKeyProviderUri
      `Prelude.seq` Prelude.rnf qopConfiguration
      `Prelude.seq` Prelude.rnf nameNodes
      `Prelude.seq` Prelude.rnf simpleUser
      `Prelude.seq` Prelude.rnf blockSize
      `Prelude.seq` Prelude.rnf kerberosKeytab
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf locationArn

instance Data.ToHeaders UpdateLocationHdfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.UpdateLocationHdfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLocationHdfs where
  toJSON UpdateLocationHdfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuthenticationType" Data..=)
              Prelude.<$> authenticationType,
            ("KerberosKrb5Conf" Data..=)
              Prelude.<$> kerberosKrb5Conf,
            ("KerberosPrincipal" Data..=)
              Prelude.<$> kerberosPrincipal,
            ("ReplicationFactor" Data..=)
              Prelude.<$> replicationFactor,
            ("KmsKeyProviderUri" Data..=)
              Prelude.<$> kmsKeyProviderUri,
            ("QopConfiguration" Data..=)
              Prelude.<$> qopConfiguration,
            ("NameNodes" Data..=) Prelude.<$> nameNodes,
            ("SimpleUser" Data..=) Prelude.<$> simpleUser,
            ("BlockSize" Data..=) Prelude.<$> blockSize,
            ("KerberosKeytab" Data..=)
              Prelude.<$> kerberosKeytab,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("AgentArns" Data..=) Prelude.<$> agentArns,
            Prelude.Just ("LocationArn" Data..= locationArn)
          ]
      )

instance Data.ToPath UpdateLocationHdfs where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLocationHdfs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLocationHdfsResponse' smart constructor.
data UpdateLocationHdfsResponse = UpdateLocationHdfsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationHdfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLocationHdfsResponse_httpStatus' - The response's http status code.
newUpdateLocationHdfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLocationHdfsResponse
newUpdateLocationHdfsResponse pHttpStatus_ =
  UpdateLocationHdfsResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLocationHdfsResponse_httpStatus :: Lens.Lens' UpdateLocationHdfsResponse Prelude.Int
updateLocationHdfsResponse_httpStatus = Lens.lens (\UpdateLocationHdfsResponse' {httpStatus} -> httpStatus) (\s@UpdateLocationHdfsResponse' {} a -> s {httpStatus = a} :: UpdateLocationHdfsResponse)

instance Prelude.NFData UpdateLocationHdfsResponse where
  rnf UpdateLocationHdfsResponse' {..} =
    Prelude.rnf httpStatus
