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
-- Module      : Amazonka.DataSync.CreateLocationHdfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a Hadoop Distributed File System (HDFS).
module Amazonka.DataSync.CreateLocationHdfs
  ( -- * Creating a Request
    CreateLocationHdfs (..),
    newCreateLocationHdfs,

    -- * Request Lenses
    createLocationHdfs_tags,
    createLocationHdfs_kerberosKrb5Conf,
    createLocationHdfs_kerberosPrincipal,
    createLocationHdfs_replicationFactor,
    createLocationHdfs_kmsKeyProviderUri,
    createLocationHdfs_qopConfiguration,
    createLocationHdfs_simpleUser,
    createLocationHdfs_blockSize,
    createLocationHdfs_kerberosKeytab,
    createLocationHdfs_subdirectory,
    createLocationHdfs_nameNodes,
    createLocationHdfs_authenticationType,
    createLocationHdfs_agentArns,

    -- * Destructuring the Response
    CreateLocationHdfsResponse (..),
    newCreateLocationHdfsResponse,

    -- * Response Lenses
    createLocationHdfsResponse_locationArn,
    createLocationHdfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocationHdfs' smart constructor.
data CreateLocationHdfs = CreateLocationHdfs'
  { -- | The key-value pair that represents the tag that you want to add to the
    -- location. The value can be an empty string. We recommend using tags to
    -- name your resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The @krb5.conf@ file that contains the Kerberos configuration
    -- information. You can load the @krb5.conf@ file by providing the file\'s
    -- address. If you\'re using the CLI, it performs the base64 encoding for
    -- you. Otherwise, provide the base64-encoded text.
    --
    -- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
    -- required.
    kerberosKrb5Conf :: Prelude.Maybe Core.Base64,
    -- | The Kerberos principal with access to the files and folders on the HDFS
    -- cluster.
    --
    -- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
    -- required.
    kerberosPrincipal :: Prelude.Maybe Prelude.Text,
    -- | The number of DataNodes to replicate the data to when writing to the
    -- HDFS cluster. By default, data is replicated to three DataNodes.
    replicationFactor :: Prelude.Maybe Prelude.Natural,
    -- | The URI of the HDFS cluster\'s Key Management Server (KMS).
    kmsKeyProviderUri :: Prelude.Maybe Prelude.Text,
    -- | The Quality of Protection (QOP) configuration specifies the Remote
    -- Procedure Call (RPC) and data transfer protection settings configured on
    -- the Hadoop Distributed File System (HDFS) cluster. If @QopConfiguration@
    -- isn\'t specified, @RpcProtection@ and @DataTransferProtection@ default
    -- to @PRIVACY@. If you set @RpcProtection@ or @DataTransferProtection@,
    -- the other parameter assumes the same value.
    qopConfiguration :: Prelude.Maybe QopConfiguration,
    -- | The user name used to identify the client on the host operating system.
    --
    -- If @SIMPLE@ is specified for @AuthenticationType@, this parameter is
    -- required.
    simpleUser :: Prelude.Maybe Prelude.Text,
    -- | The size of data blocks to write into the HDFS cluster. The block size
    -- must be a multiple of 512 bytes. The default block size is 128 mebibytes
    -- (MiB).
    blockSize :: Prelude.Maybe Prelude.Natural,
    -- | The Kerberos key table (keytab) that contains mappings between the
    -- defined Kerberos principal and the encrypted keys. You can load the
    -- keytab from a file by providing the file\'s address. If you\'re using
    -- the CLI, it performs base64 encoding for you. Otherwise, provide the
    -- base64-encoded text.
    --
    -- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
    -- required.
    kerberosKeytab :: Prelude.Maybe Core.Base64,
    -- | A subdirectory in the HDFS cluster. This subdirectory is used to read
    -- data from or write data to the HDFS cluster. If the subdirectory isn\'t
    -- specified, it will default to @\/@.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The NameNode that manages the HDFS namespace. The NameNode performs
    -- operations such as opening, closing, and renaming files and directories.
    -- The NameNode contains the information to map blocks of data to the
    -- DataNodes. You can use only one NameNode.
    nameNodes :: Prelude.NonEmpty HdfsNameNode,
    -- | The type of authentication used to determine the identity of the user.
    authenticationType :: HdfsAuthenticationType,
    -- | The Amazon Resource Names (ARNs) of the agents that are used to connect
    -- to the HDFS cluster.
    agentArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationHdfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createLocationHdfs_tags' - The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
--
-- 'kerberosKrb5Conf', 'createLocationHdfs_kerberosKrb5Conf' - The @krb5.conf@ file that contains the Kerberos configuration
-- information. You can load the @krb5.conf@ file by providing the file\'s
-- address. If you\'re using the CLI, it performs the base64 encoding for
-- you. Otherwise, provide the base64-encoded text.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'kerberosPrincipal', 'createLocationHdfs_kerberosPrincipal' - The Kerberos principal with access to the files and folders on the HDFS
-- cluster.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.
--
-- 'replicationFactor', 'createLocationHdfs_replicationFactor' - The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster. By default, data is replicated to three DataNodes.
--
-- 'kmsKeyProviderUri', 'createLocationHdfs_kmsKeyProviderUri' - The URI of the HDFS cluster\'s Key Management Server (KMS).
--
-- 'qopConfiguration', 'createLocationHdfs_qopConfiguration' - The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer protection settings configured on
-- the Hadoop Distributed File System (HDFS) cluster. If @QopConfiguration@
-- isn\'t specified, @RpcProtection@ and @DataTransferProtection@ default
-- to @PRIVACY@. If you set @RpcProtection@ or @DataTransferProtection@,
-- the other parameter assumes the same value.
--
-- 'simpleUser', 'createLocationHdfs_simpleUser' - The user name used to identify the client on the host operating system.
--
-- If @SIMPLE@ is specified for @AuthenticationType@, this parameter is
-- required.
--
-- 'blockSize', 'createLocationHdfs_blockSize' - The size of data blocks to write into the HDFS cluster. The block size
-- must be a multiple of 512 bytes. The default block size is 128 mebibytes
-- (MiB).
--
-- 'kerberosKeytab', 'createLocationHdfs_kerberosKeytab' - The Kerberos key table (keytab) that contains mappings between the
-- defined Kerberos principal and the encrypted keys. You can load the
-- keytab from a file by providing the file\'s address. If you\'re using
-- the CLI, it performs base64 encoding for you. Otherwise, provide the
-- base64-encoded text.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'subdirectory', 'createLocationHdfs_subdirectory' - A subdirectory in the HDFS cluster. This subdirectory is used to read
-- data from or write data to the HDFS cluster. If the subdirectory isn\'t
-- specified, it will default to @\/@.
--
-- 'nameNodes', 'createLocationHdfs_nameNodes' - The NameNode that manages the HDFS namespace. The NameNode performs
-- operations such as opening, closing, and renaming files and directories.
-- The NameNode contains the information to map blocks of data to the
-- DataNodes. You can use only one NameNode.
--
-- 'authenticationType', 'createLocationHdfs_authenticationType' - The type of authentication used to determine the identity of the user.
--
-- 'agentArns', 'createLocationHdfs_agentArns' - The Amazon Resource Names (ARNs) of the agents that are used to connect
-- to the HDFS cluster.
newCreateLocationHdfs ::
  -- | 'nameNodes'
  Prelude.NonEmpty HdfsNameNode ->
  -- | 'authenticationType'
  HdfsAuthenticationType ->
  -- | 'agentArns'
  Prelude.NonEmpty Prelude.Text ->
  CreateLocationHdfs
newCreateLocationHdfs
  pNameNodes_
  pAuthenticationType_
  pAgentArns_ =
    CreateLocationHdfs'
      { tags = Prelude.Nothing,
        kerberosKrb5Conf = Prelude.Nothing,
        kerberosPrincipal = Prelude.Nothing,
        replicationFactor = Prelude.Nothing,
        kmsKeyProviderUri = Prelude.Nothing,
        qopConfiguration = Prelude.Nothing,
        simpleUser = Prelude.Nothing,
        blockSize = Prelude.Nothing,
        kerberosKeytab = Prelude.Nothing,
        subdirectory = Prelude.Nothing,
        nameNodes = Lens.coerced Lens.# pNameNodes_,
        authenticationType = pAuthenticationType_,
        agentArns = Lens.coerced Lens.# pAgentArns_
      }

-- | The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
createLocationHdfs_tags :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe [TagListEntry])
createLocationHdfs_tags = Lens.lens (\CreateLocationHdfs' {tags} -> tags) (\s@CreateLocationHdfs' {} a -> s {tags = a} :: CreateLocationHdfs) Prelude.. Lens.mapping Lens.coerced

-- | The @krb5.conf@ file that contains the Kerberos configuration
-- information. You can load the @krb5.conf@ file by providing the file\'s
-- address. If you\'re using the CLI, it performs the base64 encoding for
-- you. Otherwise, provide the base64-encoded text.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createLocationHdfs_kerberosKrb5Conf :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.ByteString)
createLocationHdfs_kerberosKrb5Conf = Lens.lens (\CreateLocationHdfs' {kerberosKrb5Conf} -> kerberosKrb5Conf) (\s@CreateLocationHdfs' {} a -> s {kerberosKrb5Conf = a} :: CreateLocationHdfs) Prelude.. Lens.mapping Core._Base64

-- | The Kerberos principal with access to the files and folders on the HDFS
-- cluster.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.
createLocationHdfs_kerberosPrincipal :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Text)
createLocationHdfs_kerberosPrincipal = Lens.lens (\CreateLocationHdfs' {kerberosPrincipal} -> kerberosPrincipal) (\s@CreateLocationHdfs' {} a -> s {kerberosPrincipal = a} :: CreateLocationHdfs)

-- | The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster. By default, data is replicated to three DataNodes.
createLocationHdfs_replicationFactor :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Natural)
createLocationHdfs_replicationFactor = Lens.lens (\CreateLocationHdfs' {replicationFactor} -> replicationFactor) (\s@CreateLocationHdfs' {} a -> s {replicationFactor = a} :: CreateLocationHdfs)

-- | The URI of the HDFS cluster\'s Key Management Server (KMS).
createLocationHdfs_kmsKeyProviderUri :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Text)
createLocationHdfs_kmsKeyProviderUri = Lens.lens (\CreateLocationHdfs' {kmsKeyProviderUri} -> kmsKeyProviderUri) (\s@CreateLocationHdfs' {} a -> s {kmsKeyProviderUri = a} :: CreateLocationHdfs)

-- | The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer protection settings configured on
-- the Hadoop Distributed File System (HDFS) cluster. If @QopConfiguration@
-- isn\'t specified, @RpcProtection@ and @DataTransferProtection@ default
-- to @PRIVACY@. If you set @RpcProtection@ or @DataTransferProtection@,
-- the other parameter assumes the same value.
createLocationHdfs_qopConfiguration :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe QopConfiguration)
createLocationHdfs_qopConfiguration = Lens.lens (\CreateLocationHdfs' {qopConfiguration} -> qopConfiguration) (\s@CreateLocationHdfs' {} a -> s {qopConfiguration = a} :: CreateLocationHdfs)

-- | The user name used to identify the client on the host operating system.
--
-- If @SIMPLE@ is specified for @AuthenticationType@, this parameter is
-- required.
createLocationHdfs_simpleUser :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Text)
createLocationHdfs_simpleUser = Lens.lens (\CreateLocationHdfs' {simpleUser} -> simpleUser) (\s@CreateLocationHdfs' {} a -> s {simpleUser = a} :: CreateLocationHdfs)

-- | The size of data blocks to write into the HDFS cluster. The block size
-- must be a multiple of 512 bytes. The default block size is 128 mebibytes
-- (MiB).
createLocationHdfs_blockSize :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Natural)
createLocationHdfs_blockSize = Lens.lens (\CreateLocationHdfs' {blockSize} -> blockSize) (\s@CreateLocationHdfs' {} a -> s {blockSize = a} :: CreateLocationHdfs)

-- | The Kerberos key table (keytab) that contains mappings between the
-- defined Kerberos principal and the encrypted keys. You can load the
-- keytab from a file by providing the file\'s address. If you\'re using
-- the CLI, it performs base64 encoding for you. Otherwise, provide the
-- base64-encoded text.
--
-- If @KERBEROS@ is specified for @AuthenticationType@, this parameter is
-- required.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
createLocationHdfs_kerberosKeytab :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.ByteString)
createLocationHdfs_kerberosKeytab = Lens.lens (\CreateLocationHdfs' {kerberosKeytab} -> kerberosKeytab) (\s@CreateLocationHdfs' {} a -> s {kerberosKeytab = a} :: CreateLocationHdfs) Prelude.. Lens.mapping Core._Base64

-- | A subdirectory in the HDFS cluster. This subdirectory is used to read
-- data from or write data to the HDFS cluster. If the subdirectory isn\'t
-- specified, it will default to @\/@.
createLocationHdfs_subdirectory :: Lens.Lens' CreateLocationHdfs (Prelude.Maybe Prelude.Text)
createLocationHdfs_subdirectory = Lens.lens (\CreateLocationHdfs' {subdirectory} -> subdirectory) (\s@CreateLocationHdfs' {} a -> s {subdirectory = a} :: CreateLocationHdfs)

-- | The NameNode that manages the HDFS namespace. The NameNode performs
-- operations such as opening, closing, and renaming files and directories.
-- The NameNode contains the information to map blocks of data to the
-- DataNodes. You can use only one NameNode.
createLocationHdfs_nameNodes :: Lens.Lens' CreateLocationHdfs (Prelude.NonEmpty HdfsNameNode)
createLocationHdfs_nameNodes = Lens.lens (\CreateLocationHdfs' {nameNodes} -> nameNodes) (\s@CreateLocationHdfs' {} a -> s {nameNodes = a} :: CreateLocationHdfs) Prelude.. Lens.coerced

-- | The type of authentication used to determine the identity of the user.
createLocationHdfs_authenticationType :: Lens.Lens' CreateLocationHdfs HdfsAuthenticationType
createLocationHdfs_authenticationType = Lens.lens (\CreateLocationHdfs' {authenticationType} -> authenticationType) (\s@CreateLocationHdfs' {} a -> s {authenticationType = a} :: CreateLocationHdfs)

-- | The Amazon Resource Names (ARNs) of the agents that are used to connect
-- to the HDFS cluster.
createLocationHdfs_agentArns :: Lens.Lens' CreateLocationHdfs (Prelude.NonEmpty Prelude.Text)
createLocationHdfs_agentArns = Lens.lens (\CreateLocationHdfs' {agentArns} -> agentArns) (\s@CreateLocationHdfs' {} a -> s {agentArns = a} :: CreateLocationHdfs) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLocationHdfs where
  type
    AWSResponse CreateLocationHdfs =
      CreateLocationHdfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationHdfsResponse'
            Prelude.<$> (x Core..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationHdfs where
  hashWithSalt _salt CreateLocationHdfs' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` kerberosKrb5Conf
      `Prelude.hashWithSalt` kerberosPrincipal
      `Prelude.hashWithSalt` replicationFactor
      `Prelude.hashWithSalt` kmsKeyProviderUri
      `Prelude.hashWithSalt` qopConfiguration
      `Prelude.hashWithSalt` simpleUser
      `Prelude.hashWithSalt` blockSize
      `Prelude.hashWithSalt` kerberosKeytab
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` nameNodes
      `Prelude.hashWithSalt` authenticationType
      `Prelude.hashWithSalt` agentArns

instance Prelude.NFData CreateLocationHdfs where
  rnf CreateLocationHdfs' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf kerberosKrb5Conf
      `Prelude.seq` Prelude.rnf kerberosPrincipal
      `Prelude.seq` Prelude.rnf replicationFactor
      `Prelude.seq` Prelude.rnf kmsKeyProviderUri
      `Prelude.seq` Prelude.rnf qopConfiguration
      `Prelude.seq` Prelude.rnf simpleUser
      `Prelude.seq` Prelude.rnf blockSize
      `Prelude.seq` Prelude.rnf kerberosKeytab
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf nameNodes
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf agentArns

instance Core.ToHeaders CreateLocationHdfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.CreateLocationHdfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLocationHdfs where
  toJSON CreateLocationHdfs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Tags" Core..=) Prelude.<$> tags,
            ("KerberosKrb5Conf" Core..=)
              Prelude.<$> kerberosKrb5Conf,
            ("KerberosPrincipal" Core..=)
              Prelude.<$> kerberosPrincipal,
            ("ReplicationFactor" Core..=)
              Prelude.<$> replicationFactor,
            ("KmsKeyProviderUri" Core..=)
              Prelude.<$> kmsKeyProviderUri,
            ("QopConfiguration" Core..=)
              Prelude.<$> qopConfiguration,
            ("SimpleUser" Core..=) Prelude.<$> simpleUser,
            ("BlockSize" Core..=) Prelude.<$> blockSize,
            ("KerberosKeytab" Core..=)
              Prelude.<$> kerberosKeytab,
            ("Subdirectory" Core..=) Prelude.<$> subdirectory,
            Prelude.Just ("NameNodes" Core..= nameNodes),
            Prelude.Just
              ("AuthenticationType" Core..= authenticationType),
            Prelude.Just ("AgentArns" Core..= agentArns)
          ]
      )

instance Core.ToPath CreateLocationHdfs where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLocationHdfs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLocationHdfsResponse' smart constructor.
data CreateLocationHdfsResponse = CreateLocationHdfsResponse'
  { -- | The ARN of the source HDFS cluster location that\'s created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationHdfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationHdfsResponse_locationArn' - The ARN of the source HDFS cluster location that\'s created.
--
-- 'httpStatus', 'createLocationHdfsResponse_httpStatus' - The response's http status code.
newCreateLocationHdfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationHdfsResponse
newCreateLocationHdfsResponse pHttpStatus_ =
  CreateLocationHdfsResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the source HDFS cluster location that\'s created.
createLocationHdfsResponse_locationArn :: Lens.Lens' CreateLocationHdfsResponse (Prelude.Maybe Prelude.Text)
createLocationHdfsResponse_locationArn = Lens.lens (\CreateLocationHdfsResponse' {locationArn} -> locationArn) (\s@CreateLocationHdfsResponse' {} a -> s {locationArn = a} :: CreateLocationHdfsResponse)

-- | The response's http status code.
createLocationHdfsResponse_httpStatus :: Lens.Lens' CreateLocationHdfsResponse Prelude.Int
createLocationHdfsResponse_httpStatus = Lens.lens (\CreateLocationHdfsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationHdfsResponse' {} a -> s {httpStatus = a} :: CreateLocationHdfsResponse)

instance Prelude.NFData CreateLocationHdfsResponse where
  rnf CreateLocationHdfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
