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
-- Module      : Amazonka.Redshift.CreateCustomDomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to create a custom domain name for a cluster. Properties include
-- the custom domain name, the cluster the custom domain is associated
-- with, and the certificate Amazon Resource Name (ARN).
module Amazonka.Redshift.CreateCustomDomainAssociation
  ( -- * Creating a Request
    CreateCustomDomainAssociation (..),
    newCreateCustomDomainAssociation,

    -- * Request Lenses
    createCustomDomainAssociation_customDomainName,
    createCustomDomainAssociation_customDomainCertificateArn,
    createCustomDomainAssociation_clusterIdentifier,

    -- * Destructuring the Response
    CreateCustomDomainAssociationResponse (..),
    newCreateCustomDomainAssociationResponse,

    -- * Response Lenses
    createCustomDomainAssociationResponse_clusterIdentifier,
    createCustomDomainAssociationResponse_customDomainCertExpiryTime,
    createCustomDomainAssociationResponse_customDomainCertificateArn,
    createCustomDomainAssociationResponse_customDomainName,
    createCustomDomainAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateCustomDomainAssociation' smart constructor.
data CreateCustomDomainAssociation = CreateCustomDomainAssociation'
  { -- | The custom domain name for a custom domain association.
    customDomainName :: Prelude.Text,
    -- | The certificate Amazon Resource Name (ARN) for the custom domain name
    -- association.
    customDomainCertificateArn :: Prelude.Text,
    -- | The cluster identifier that the custom domain is associated with.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDomainName', 'createCustomDomainAssociation_customDomainName' - The custom domain name for a custom domain association.
--
-- 'customDomainCertificateArn', 'createCustomDomainAssociation_customDomainCertificateArn' - The certificate Amazon Resource Name (ARN) for the custom domain name
-- association.
--
-- 'clusterIdentifier', 'createCustomDomainAssociation_clusterIdentifier' - The cluster identifier that the custom domain is associated with.
newCreateCustomDomainAssociation ::
  -- | 'customDomainName'
  Prelude.Text ->
  -- | 'customDomainCertificateArn'
  Prelude.Text ->
  -- | 'clusterIdentifier'
  Prelude.Text ->
  CreateCustomDomainAssociation
newCreateCustomDomainAssociation
  pCustomDomainName_
  pCustomDomainCertificateArn_
  pClusterIdentifier_ =
    CreateCustomDomainAssociation'
      { customDomainName =
          pCustomDomainName_,
        customDomainCertificateArn =
          pCustomDomainCertificateArn_,
        clusterIdentifier = pClusterIdentifier_
      }

-- | The custom domain name for a custom domain association.
createCustomDomainAssociation_customDomainName :: Lens.Lens' CreateCustomDomainAssociation Prelude.Text
createCustomDomainAssociation_customDomainName = Lens.lens (\CreateCustomDomainAssociation' {customDomainName} -> customDomainName) (\s@CreateCustomDomainAssociation' {} a -> s {customDomainName = a} :: CreateCustomDomainAssociation)

-- | The certificate Amazon Resource Name (ARN) for the custom domain name
-- association.
createCustomDomainAssociation_customDomainCertificateArn :: Lens.Lens' CreateCustomDomainAssociation Prelude.Text
createCustomDomainAssociation_customDomainCertificateArn = Lens.lens (\CreateCustomDomainAssociation' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@CreateCustomDomainAssociation' {} a -> s {customDomainCertificateArn = a} :: CreateCustomDomainAssociation)

-- | The cluster identifier that the custom domain is associated with.
createCustomDomainAssociation_clusterIdentifier :: Lens.Lens' CreateCustomDomainAssociation Prelude.Text
createCustomDomainAssociation_clusterIdentifier = Lens.lens (\CreateCustomDomainAssociation' {clusterIdentifier} -> clusterIdentifier) (\s@CreateCustomDomainAssociation' {} a -> s {clusterIdentifier = a} :: CreateCustomDomainAssociation)

instance
  Core.AWSRequest
    CreateCustomDomainAssociation
  where
  type
    AWSResponse CreateCustomDomainAssociation =
      CreateCustomDomainAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateCustomDomainAssociationResult"
      ( \s h x ->
          CreateCustomDomainAssociationResponse'
            Prelude.<$> (x Data..@? "ClusterIdentifier")
            Prelude.<*> (x Data..@? "CustomDomainCertExpiryTime")
            Prelude.<*> (x Data..@? "CustomDomainCertificateArn")
            Prelude.<*> (x Data..@? "CustomDomainName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateCustomDomainAssociation
  where
  hashWithSalt _salt CreateCustomDomainAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` customDomainName
      `Prelude.hashWithSalt` customDomainCertificateArn
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData CreateCustomDomainAssociation where
  rnf CreateCustomDomainAssociation' {..} =
    Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders CreateCustomDomainAssociation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateCustomDomainAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCustomDomainAssociation where
  toQuery CreateCustomDomainAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateCustomDomainAssociation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "CustomDomainName" Data.=: customDomainName,
        "CustomDomainCertificateArn"
          Data.=: customDomainCertificateArn,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newCreateCustomDomainAssociationResponse' smart constructor.
data CreateCustomDomainAssociationResponse = CreateCustomDomainAssociationResponse'
  { -- | The identifier of the cluster that the custom domain is associated with.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The expiration time for the certificate for the custom domain.
    customDomainCertExpiryTime :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the certificate associated with the
    -- custom domain name.
    customDomainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name for the association result.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCustomDomainAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'createCustomDomainAssociationResponse_clusterIdentifier' - The identifier of the cluster that the custom domain is associated with.
--
-- 'customDomainCertExpiryTime', 'createCustomDomainAssociationResponse_customDomainCertExpiryTime' - The expiration time for the certificate for the custom domain.
--
-- 'customDomainCertificateArn', 'createCustomDomainAssociationResponse_customDomainCertificateArn' - The Amazon Resource Name (ARN) for the certificate associated with the
-- custom domain name.
--
-- 'customDomainName', 'createCustomDomainAssociationResponse_customDomainName' - The custom domain name for the association result.
--
-- 'httpStatus', 'createCustomDomainAssociationResponse_httpStatus' - The response's http status code.
newCreateCustomDomainAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateCustomDomainAssociationResponse
newCreateCustomDomainAssociationResponse pHttpStatus_ =
  CreateCustomDomainAssociationResponse'
    { clusterIdentifier =
        Prelude.Nothing,
      customDomainCertExpiryTime =
        Prelude.Nothing,
      customDomainCertificateArn =
        Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the cluster that the custom domain is associated with.
createCustomDomainAssociationResponse_clusterIdentifier :: Lens.Lens' CreateCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
createCustomDomainAssociationResponse_clusterIdentifier = Lens.lens (\CreateCustomDomainAssociationResponse' {clusterIdentifier} -> clusterIdentifier) (\s@CreateCustomDomainAssociationResponse' {} a -> s {clusterIdentifier = a} :: CreateCustomDomainAssociationResponse)

-- | The expiration time for the certificate for the custom domain.
createCustomDomainAssociationResponse_customDomainCertExpiryTime :: Lens.Lens' CreateCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
createCustomDomainAssociationResponse_customDomainCertExpiryTime = Lens.lens (\CreateCustomDomainAssociationResponse' {customDomainCertExpiryTime} -> customDomainCertExpiryTime) (\s@CreateCustomDomainAssociationResponse' {} a -> s {customDomainCertExpiryTime = a} :: CreateCustomDomainAssociationResponse)

-- | The Amazon Resource Name (ARN) for the certificate associated with the
-- custom domain name.
createCustomDomainAssociationResponse_customDomainCertificateArn :: Lens.Lens' CreateCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
createCustomDomainAssociationResponse_customDomainCertificateArn = Lens.lens (\CreateCustomDomainAssociationResponse' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@CreateCustomDomainAssociationResponse' {} a -> s {customDomainCertificateArn = a} :: CreateCustomDomainAssociationResponse)

-- | The custom domain name for the association result.
createCustomDomainAssociationResponse_customDomainName :: Lens.Lens' CreateCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
createCustomDomainAssociationResponse_customDomainName = Lens.lens (\CreateCustomDomainAssociationResponse' {customDomainName} -> customDomainName) (\s@CreateCustomDomainAssociationResponse' {} a -> s {customDomainName = a} :: CreateCustomDomainAssociationResponse)

-- | The response's http status code.
createCustomDomainAssociationResponse_httpStatus :: Lens.Lens' CreateCustomDomainAssociationResponse Prelude.Int
createCustomDomainAssociationResponse_httpStatus = Lens.lens (\CreateCustomDomainAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateCustomDomainAssociationResponse' {} a -> s {httpStatus = a} :: CreateCustomDomainAssociationResponse)

instance
  Prelude.NFData
    CreateCustomDomainAssociationResponse
  where
  rnf CreateCustomDomainAssociationResponse' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf customDomainCertExpiryTime
      `Prelude.seq` Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf httpStatus
