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
-- Module      : Amazonka.DataSync.DescribeLocationHdfs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the authentication information about the
-- Hadoop Distributed File System (HDFS) location.
module Amazonka.DataSync.DescribeLocationHdfs
  ( -- * Creating a Request
    DescribeLocationHdfs (..),
    newDescribeLocationHdfs,

    -- * Request Lenses
    describeLocationHdfs_locationArn,

    -- * Destructuring the Response
    DescribeLocationHdfsResponse (..),
    newDescribeLocationHdfsResponse,

    -- * Response Lenses
    describeLocationHdfsResponse_agentArns,
    describeLocationHdfsResponse_authenticationType,
    describeLocationHdfsResponse_blockSize,
    describeLocationHdfsResponse_creationTime,
    describeLocationHdfsResponse_kerberosPrincipal,
    describeLocationHdfsResponse_kmsKeyProviderUri,
    describeLocationHdfsResponse_locationArn,
    describeLocationHdfsResponse_locationUri,
    describeLocationHdfsResponse_nameNodes,
    describeLocationHdfsResponse_qopConfiguration,
    describeLocationHdfsResponse_replicationFactor,
    describeLocationHdfsResponse_simpleUser,
    describeLocationHdfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLocationHdfs' smart constructor.
data DescribeLocationHdfs = DescribeLocationHdfs'
  { -- | The Amazon Resource Name (ARN) of the HDFS cluster location to describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationHdfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationHdfs_locationArn' - The Amazon Resource Name (ARN) of the HDFS cluster location to describe.
newDescribeLocationHdfs ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationHdfs
newDescribeLocationHdfs pLocationArn_ =
  DescribeLocationHdfs' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the HDFS cluster location to describe.
describeLocationHdfs_locationArn :: Lens.Lens' DescribeLocationHdfs Prelude.Text
describeLocationHdfs_locationArn = Lens.lens (\DescribeLocationHdfs' {locationArn} -> locationArn) (\s@DescribeLocationHdfs' {} a -> s {locationArn = a} :: DescribeLocationHdfs)

instance Core.AWSRequest DescribeLocationHdfs where
  type
    AWSResponse DescribeLocationHdfs =
      DescribeLocationHdfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationHdfsResponse'
            Prelude.<$> (x Data..?> "AgentArns")
            Prelude.<*> (x Data..?> "AuthenticationType")
            Prelude.<*> (x Data..?> "BlockSize")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "KerberosPrincipal")
            Prelude.<*> (x Data..?> "KmsKeyProviderUri")
            Prelude.<*> (x Data..?> "LocationArn")
            Prelude.<*> (x Data..?> "LocationUri")
            Prelude.<*> (x Data..?> "NameNodes")
            Prelude.<*> (x Data..?> "QopConfiguration")
            Prelude.<*> (x Data..?> "ReplicationFactor")
            Prelude.<*> (x Data..?> "SimpleUser")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationHdfs where
  hashWithSalt _salt DescribeLocationHdfs' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationHdfs where
  rnf DescribeLocationHdfs' {..} =
    Prelude.rnf locationArn

instance Data.ToHeaders DescribeLocationHdfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeLocationHdfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLocationHdfs where
  toJSON DescribeLocationHdfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Data..= locationArn)]
      )

instance Data.ToPath DescribeLocationHdfs where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocationHdfs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLocationHdfsResponse' smart constructor.
data DescribeLocationHdfsResponse = DescribeLocationHdfsResponse'
  { -- | The ARNs of the agents that are used to connect to the HDFS cluster.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The type of authentication used to determine the identity of the user.
    authenticationType :: Prelude.Maybe HdfsAuthenticationType,
    -- | The size of the data blocks to write into the HDFS cluster.
    blockSize :: Prelude.Maybe Prelude.Natural,
    -- | The time that the HDFS location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Kerberos principal with access to the files and folders on the HDFS
    -- cluster. This parameter is used if the @AuthenticationType@ is defined
    -- as @KERBEROS@.
    kerberosPrincipal :: Prelude.Maybe Prelude.Text,
    -- | The URI of the HDFS cluster\'s Key Management Server (KMS).
    kmsKeyProviderUri :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the HDFS cluster location.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The URI of the HDFS cluster location.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The NameNode that manage the HDFS namespace.
    nameNodes :: Prelude.Maybe (Prelude.NonEmpty HdfsNameNode),
    -- | The Quality of Protection (QOP) configuration specifies the Remote
    -- Procedure Call (RPC) and data transfer protection settings configured on
    -- the Hadoop Distributed File System (HDFS) cluster.
    qopConfiguration :: Prelude.Maybe QopConfiguration,
    -- | The number of DataNodes to replicate the data to when writing to the
    -- HDFS cluster.
    replicationFactor :: Prelude.Maybe Prelude.Natural,
    -- | The user name used to identify the client on the host operating system.
    -- This parameter is used if the @AuthenticationType@ is defined as
    -- @SIMPLE@.
    simpleUser :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationHdfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'describeLocationHdfsResponse_agentArns' - The ARNs of the agents that are used to connect to the HDFS cluster.
--
-- 'authenticationType', 'describeLocationHdfsResponse_authenticationType' - The type of authentication used to determine the identity of the user.
--
-- 'blockSize', 'describeLocationHdfsResponse_blockSize' - The size of the data blocks to write into the HDFS cluster.
--
-- 'creationTime', 'describeLocationHdfsResponse_creationTime' - The time that the HDFS location was created.
--
-- 'kerberosPrincipal', 'describeLocationHdfsResponse_kerberosPrincipal' - The Kerberos principal with access to the files and folders on the HDFS
-- cluster. This parameter is used if the @AuthenticationType@ is defined
-- as @KERBEROS@.
--
-- 'kmsKeyProviderUri', 'describeLocationHdfsResponse_kmsKeyProviderUri' - The URI of the HDFS cluster\'s Key Management Server (KMS).
--
-- 'locationArn', 'describeLocationHdfsResponse_locationArn' - The ARN of the HDFS cluster location.
--
-- 'locationUri', 'describeLocationHdfsResponse_locationUri' - The URI of the HDFS cluster location.
--
-- 'nameNodes', 'describeLocationHdfsResponse_nameNodes' - The NameNode that manage the HDFS namespace.
--
-- 'qopConfiguration', 'describeLocationHdfsResponse_qopConfiguration' - The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer protection settings configured on
-- the Hadoop Distributed File System (HDFS) cluster.
--
-- 'replicationFactor', 'describeLocationHdfsResponse_replicationFactor' - The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster.
--
-- 'simpleUser', 'describeLocationHdfsResponse_simpleUser' - The user name used to identify the client on the host operating system.
-- This parameter is used if the @AuthenticationType@ is defined as
-- @SIMPLE@.
--
-- 'httpStatus', 'describeLocationHdfsResponse_httpStatus' - The response's http status code.
newDescribeLocationHdfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationHdfsResponse
newDescribeLocationHdfsResponse pHttpStatus_ =
  DescribeLocationHdfsResponse'
    { agentArns =
        Prelude.Nothing,
      authenticationType = Prelude.Nothing,
      blockSize = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      kerberosPrincipal = Prelude.Nothing,
      kmsKeyProviderUri = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      nameNodes = Prelude.Nothing,
      qopConfiguration = Prelude.Nothing,
      replicationFactor = Prelude.Nothing,
      simpleUser = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARNs of the agents that are used to connect to the HDFS cluster.
describeLocationHdfsResponse_agentArns :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationHdfsResponse_agentArns = Lens.lens (\DescribeLocationHdfsResponse' {agentArns} -> agentArns) (\s@DescribeLocationHdfsResponse' {} a -> s {agentArns = a} :: DescribeLocationHdfsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The type of authentication used to determine the identity of the user.
describeLocationHdfsResponse_authenticationType :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe HdfsAuthenticationType)
describeLocationHdfsResponse_authenticationType = Lens.lens (\DescribeLocationHdfsResponse' {authenticationType} -> authenticationType) (\s@DescribeLocationHdfsResponse' {} a -> s {authenticationType = a} :: DescribeLocationHdfsResponse)

-- | The size of the data blocks to write into the HDFS cluster.
describeLocationHdfsResponse_blockSize :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Natural)
describeLocationHdfsResponse_blockSize = Lens.lens (\DescribeLocationHdfsResponse' {blockSize} -> blockSize) (\s@DescribeLocationHdfsResponse' {} a -> s {blockSize = a} :: DescribeLocationHdfsResponse)

-- | The time that the HDFS location was created.
describeLocationHdfsResponse_creationTime :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationHdfsResponse_creationTime = Lens.lens (\DescribeLocationHdfsResponse' {creationTime} -> creationTime) (\s@DescribeLocationHdfsResponse' {} a -> s {creationTime = a} :: DescribeLocationHdfsResponse) Prelude.. Lens.mapping Data._Time

-- | The Kerberos principal with access to the files and folders on the HDFS
-- cluster. This parameter is used if the @AuthenticationType@ is defined
-- as @KERBEROS@.
describeLocationHdfsResponse_kerberosPrincipal :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Text)
describeLocationHdfsResponse_kerberosPrincipal = Lens.lens (\DescribeLocationHdfsResponse' {kerberosPrincipal} -> kerberosPrincipal) (\s@DescribeLocationHdfsResponse' {} a -> s {kerberosPrincipal = a} :: DescribeLocationHdfsResponse)

-- | The URI of the HDFS cluster\'s Key Management Server (KMS).
describeLocationHdfsResponse_kmsKeyProviderUri :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Text)
describeLocationHdfsResponse_kmsKeyProviderUri = Lens.lens (\DescribeLocationHdfsResponse' {kmsKeyProviderUri} -> kmsKeyProviderUri) (\s@DescribeLocationHdfsResponse' {} a -> s {kmsKeyProviderUri = a} :: DescribeLocationHdfsResponse)

-- | The ARN of the HDFS cluster location.
describeLocationHdfsResponse_locationArn :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Text)
describeLocationHdfsResponse_locationArn = Lens.lens (\DescribeLocationHdfsResponse' {locationArn} -> locationArn) (\s@DescribeLocationHdfsResponse' {} a -> s {locationArn = a} :: DescribeLocationHdfsResponse)

-- | The URI of the HDFS cluster location.
describeLocationHdfsResponse_locationUri :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Text)
describeLocationHdfsResponse_locationUri = Lens.lens (\DescribeLocationHdfsResponse' {locationUri} -> locationUri) (\s@DescribeLocationHdfsResponse' {} a -> s {locationUri = a} :: DescribeLocationHdfsResponse)

-- | The NameNode that manage the HDFS namespace.
describeLocationHdfsResponse_nameNodes :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe (Prelude.NonEmpty HdfsNameNode))
describeLocationHdfsResponse_nameNodes = Lens.lens (\DescribeLocationHdfsResponse' {nameNodes} -> nameNodes) (\s@DescribeLocationHdfsResponse' {} a -> s {nameNodes = a} :: DescribeLocationHdfsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Quality of Protection (QOP) configuration specifies the Remote
-- Procedure Call (RPC) and data transfer protection settings configured on
-- the Hadoop Distributed File System (HDFS) cluster.
describeLocationHdfsResponse_qopConfiguration :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe QopConfiguration)
describeLocationHdfsResponse_qopConfiguration = Lens.lens (\DescribeLocationHdfsResponse' {qopConfiguration} -> qopConfiguration) (\s@DescribeLocationHdfsResponse' {} a -> s {qopConfiguration = a} :: DescribeLocationHdfsResponse)

-- | The number of DataNodes to replicate the data to when writing to the
-- HDFS cluster.
describeLocationHdfsResponse_replicationFactor :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Natural)
describeLocationHdfsResponse_replicationFactor = Lens.lens (\DescribeLocationHdfsResponse' {replicationFactor} -> replicationFactor) (\s@DescribeLocationHdfsResponse' {} a -> s {replicationFactor = a} :: DescribeLocationHdfsResponse)

-- | The user name used to identify the client on the host operating system.
-- This parameter is used if the @AuthenticationType@ is defined as
-- @SIMPLE@.
describeLocationHdfsResponse_simpleUser :: Lens.Lens' DescribeLocationHdfsResponse (Prelude.Maybe Prelude.Text)
describeLocationHdfsResponse_simpleUser = Lens.lens (\DescribeLocationHdfsResponse' {simpleUser} -> simpleUser) (\s@DescribeLocationHdfsResponse' {} a -> s {simpleUser = a} :: DescribeLocationHdfsResponse)

-- | The response's http status code.
describeLocationHdfsResponse_httpStatus :: Lens.Lens' DescribeLocationHdfsResponse Prelude.Int
describeLocationHdfsResponse_httpStatus = Lens.lens (\DescribeLocationHdfsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationHdfsResponse' {} a -> s {httpStatus = a} :: DescribeLocationHdfsResponse)

instance Prelude.NFData DescribeLocationHdfsResponse where
  rnf DescribeLocationHdfsResponse' {..} =
    Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf authenticationType
      `Prelude.seq` Prelude.rnf blockSize
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf kerberosPrincipal
      `Prelude.seq` Prelude.rnf kmsKeyProviderUri
      `Prelude.seq` Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf nameNodes
      `Prelude.seq` Prelude.rnf qopConfiguration
      `Prelude.seq` Prelude.rnf replicationFactor
      `Prelude.seq` Prelude.rnf simpleUser
      `Prelude.seq` Prelude.rnf httpStatus
