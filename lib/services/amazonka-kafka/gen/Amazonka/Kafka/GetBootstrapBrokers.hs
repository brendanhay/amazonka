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
-- Module      : Amazonka.Kafka.GetBootstrapBrokers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of brokers that a client application can use to bootstrap.
module Amazonka.Kafka.GetBootstrapBrokers
  ( -- * Creating a Request
    GetBootstrapBrokers (..),
    newGetBootstrapBrokers,

    -- * Request Lenses
    getBootstrapBrokers_clusterArn,

    -- * Destructuring the Response
    GetBootstrapBrokersResponse (..),
    newGetBootstrapBrokersResponse,

    -- * Response Lenses
    getBootstrapBrokersResponse_bootstrapBrokerString,
    getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslIam,
    getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslScram,
    getBootstrapBrokersResponse_bootstrapBrokerStringPublicTls,
    getBootstrapBrokersResponse_bootstrapBrokerStringSaslIam,
    getBootstrapBrokersResponse_bootstrapBrokerStringSaslScram,
    getBootstrapBrokersResponse_bootstrapBrokerStringTls,
    getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslIam,
    getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslScram,
    getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivityTls,
    getBootstrapBrokersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBootstrapBrokers' smart constructor.
data GetBootstrapBrokers = GetBootstrapBrokers'
  { -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBootstrapBrokers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'getBootstrapBrokers_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
newGetBootstrapBrokers ::
  -- | 'clusterArn'
  Prelude.Text ->
  GetBootstrapBrokers
newGetBootstrapBrokers pClusterArn_ =
  GetBootstrapBrokers' {clusterArn = pClusterArn_}

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
getBootstrapBrokers_clusterArn :: Lens.Lens' GetBootstrapBrokers Prelude.Text
getBootstrapBrokers_clusterArn = Lens.lens (\GetBootstrapBrokers' {clusterArn} -> clusterArn) (\s@GetBootstrapBrokers' {} a -> s {clusterArn = a} :: GetBootstrapBrokers)

instance Core.AWSRequest GetBootstrapBrokers where
  type
    AWSResponse GetBootstrapBrokers =
      GetBootstrapBrokersResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBootstrapBrokersResponse'
            Prelude.<$> (x Data..?> "bootstrapBrokerString")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringPublicSaslIam")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringPublicSaslScram")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringPublicTls")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringSaslIam")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringSaslScram")
            Prelude.<*> (x Data..?> "bootstrapBrokerStringTls")
            Prelude.<*> ( x
                            Data..?> "bootstrapBrokerStringVpcConnectivitySaslIam"
                        )
            Prelude.<*> ( x
                            Data..?> "bootstrapBrokerStringVpcConnectivitySaslScram"
                        )
            Prelude.<*> ( x
                            Data..?> "bootstrapBrokerStringVpcConnectivityTls"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBootstrapBrokers where
  hashWithSalt _salt GetBootstrapBrokers' {..} =
    _salt `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData GetBootstrapBrokers where
  rnf GetBootstrapBrokers' {..} = Prelude.rnf clusterArn

instance Data.ToHeaders GetBootstrapBrokers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetBootstrapBrokers where
  toPath GetBootstrapBrokers' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/bootstrap-brokers"
      ]

instance Data.ToQuery GetBootstrapBrokers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBootstrapBrokersResponse' smart constructor.
data GetBootstrapBrokersResponse = GetBootstrapBrokersResponse'
  { -- | A string containing one or more hostname:port pairs.
    bootstrapBrokerString :: Prelude.Maybe Prelude.Text,
    -- | A string that contains one or more DNS names (or IP addresses) and SASL
    -- IAM port pairs.
    bootstrapBrokerStringPublicSaslIam :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and Sasl Scram port
    -- pairs.
    bootstrapBrokerStringPublicSaslScram :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and TLS port pairs.
    bootstrapBrokerStringPublicTls :: Prelude.Maybe Prelude.Text,
    -- | A string that contains one or more DNS names (or IP addresses) and SASL
    -- IAM port pairs.
    bootstrapBrokerStringSaslIam :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and Sasl Scram port
    -- pairs.
    bootstrapBrokerStringSaslScram :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and TLS port pairs.
    bootstrapBrokerStringTls :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and SASL\/IAM port
    -- pairs for VPC connectivity.
    bootstrapBrokerStringVpcConnectivitySaslIam :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and SASL\/SCRAM port
    -- pairs for VPC connectivity.
    bootstrapBrokerStringVpcConnectivitySaslScram :: Prelude.Maybe Prelude.Text,
    -- | A string containing one or more DNS names (or IP) and TLS port pairs for
    -- VPC connectivity.
    bootstrapBrokerStringVpcConnectivityTls :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBootstrapBrokersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bootstrapBrokerString', 'getBootstrapBrokersResponse_bootstrapBrokerString' - A string containing one or more hostname:port pairs.
--
-- 'bootstrapBrokerStringPublicSaslIam', 'getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslIam' - A string that contains one or more DNS names (or IP addresses) and SASL
-- IAM port pairs.
--
-- 'bootstrapBrokerStringPublicSaslScram', 'getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslScram' - A string containing one or more DNS names (or IP) and Sasl Scram port
-- pairs.
--
-- 'bootstrapBrokerStringPublicTls', 'getBootstrapBrokersResponse_bootstrapBrokerStringPublicTls' - A string containing one or more DNS names (or IP) and TLS port pairs.
--
-- 'bootstrapBrokerStringSaslIam', 'getBootstrapBrokersResponse_bootstrapBrokerStringSaslIam' - A string that contains one or more DNS names (or IP addresses) and SASL
-- IAM port pairs.
--
-- 'bootstrapBrokerStringSaslScram', 'getBootstrapBrokersResponse_bootstrapBrokerStringSaslScram' - A string containing one or more DNS names (or IP) and Sasl Scram port
-- pairs.
--
-- 'bootstrapBrokerStringTls', 'getBootstrapBrokersResponse_bootstrapBrokerStringTls' - A string containing one or more DNS names (or IP) and TLS port pairs.
--
-- 'bootstrapBrokerStringVpcConnectivitySaslIam', 'getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslIam' - A string containing one or more DNS names (or IP) and SASL\/IAM port
-- pairs for VPC connectivity.
--
-- 'bootstrapBrokerStringVpcConnectivitySaslScram', 'getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslScram' - A string containing one or more DNS names (or IP) and SASL\/SCRAM port
-- pairs for VPC connectivity.
--
-- 'bootstrapBrokerStringVpcConnectivityTls', 'getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivityTls' - A string containing one or more DNS names (or IP) and TLS port pairs for
-- VPC connectivity.
--
-- 'httpStatus', 'getBootstrapBrokersResponse_httpStatus' - The response's http status code.
newGetBootstrapBrokersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBootstrapBrokersResponse
newGetBootstrapBrokersResponse pHttpStatus_ =
  GetBootstrapBrokersResponse'
    { bootstrapBrokerString =
        Prelude.Nothing,
      bootstrapBrokerStringPublicSaslIam =
        Prelude.Nothing,
      bootstrapBrokerStringPublicSaslScram =
        Prelude.Nothing,
      bootstrapBrokerStringPublicTls =
        Prelude.Nothing,
      bootstrapBrokerStringSaslIam = Prelude.Nothing,
      bootstrapBrokerStringSaslScram =
        Prelude.Nothing,
      bootstrapBrokerStringTls = Prelude.Nothing,
      bootstrapBrokerStringVpcConnectivitySaslIam =
        Prelude.Nothing,
      bootstrapBrokerStringVpcConnectivitySaslScram =
        Prelude.Nothing,
      bootstrapBrokerStringVpcConnectivityTls =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string containing one or more hostname:port pairs.
getBootstrapBrokersResponse_bootstrapBrokerString :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerString = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerString} -> bootstrapBrokerString) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerString = a} :: GetBootstrapBrokersResponse)

-- | A string that contains one or more DNS names (or IP addresses) and SASL
-- IAM port pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslIam :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslIam = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringPublicSaslIam} -> bootstrapBrokerStringPublicSaslIam) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringPublicSaslIam = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and Sasl Scram port
-- pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslScram :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringPublicSaslScram = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringPublicSaslScram} -> bootstrapBrokerStringPublicSaslScram) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringPublicSaslScram = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and TLS port pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringPublicTls :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringPublicTls = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringPublicTls} -> bootstrapBrokerStringPublicTls) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringPublicTls = a} :: GetBootstrapBrokersResponse)

-- | A string that contains one or more DNS names (or IP addresses) and SASL
-- IAM port pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringSaslIam :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringSaslIam = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringSaslIam} -> bootstrapBrokerStringSaslIam) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringSaslIam = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and Sasl Scram port
-- pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringSaslScram :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringSaslScram = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringSaslScram} -> bootstrapBrokerStringSaslScram) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringSaslScram = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and TLS port pairs.
getBootstrapBrokersResponse_bootstrapBrokerStringTls :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringTls = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringTls} -> bootstrapBrokerStringTls) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringTls = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and SASL\/IAM port
-- pairs for VPC connectivity.
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslIam :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslIam = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringVpcConnectivitySaslIam} -> bootstrapBrokerStringVpcConnectivitySaslIam) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringVpcConnectivitySaslIam = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and SASL\/SCRAM port
-- pairs for VPC connectivity.
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslScram :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivitySaslScram = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringVpcConnectivitySaslScram} -> bootstrapBrokerStringVpcConnectivitySaslScram) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringVpcConnectivitySaslScram = a} :: GetBootstrapBrokersResponse)

-- | A string containing one or more DNS names (or IP) and TLS port pairs for
-- VPC connectivity.
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivityTls :: Lens.Lens' GetBootstrapBrokersResponse (Prelude.Maybe Prelude.Text)
getBootstrapBrokersResponse_bootstrapBrokerStringVpcConnectivityTls = Lens.lens (\GetBootstrapBrokersResponse' {bootstrapBrokerStringVpcConnectivityTls} -> bootstrapBrokerStringVpcConnectivityTls) (\s@GetBootstrapBrokersResponse' {} a -> s {bootstrapBrokerStringVpcConnectivityTls = a} :: GetBootstrapBrokersResponse)

-- | The response's http status code.
getBootstrapBrokersResponse_httpStatus :: Lens.Lens' GetBootstrapBrokersResponse Prelude.Int
getBootstrapBrokersResponse_httpStatus = Lens.lens (\GetBootstrapBrokersResponse' {httpStatus} -> httpStatus) (\s@GetBootstrapBrokersResponse' {} a -> s {httpStatus = a} :: GetBootstrapBrokersResponse)

instance Prelude.NFData GetBootstrapBrokersResponse where
  rnf GetBootstrapBrokersResponse' {..} =
    Prelude.rnf bootstrapBrokerString
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringPublicSaslIam
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringPublicSaslScram
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringPublicTls
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringSaslIam
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringSaslScram
      `Prelude.seq` Prelude.rnf bootstrapBrokerStringTls
      `Prelude.seq` Prelude.rnf
        bootstrapBrokerStringVpcConnectivitySaslIam
      `Prelude.seq` Prelude.rnf
        bootstrapBrokerStringVpcConnectivitySaslScram
      `Prelude.seq` Prelude.rnf
        bootstrapBrokerStringVpcConnectivityTls
      `Prelude.seq` Prelude.rnf httpStatus
