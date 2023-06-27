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
-- Module      : Amazonka.Redshift.ModifyCustomDomainAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information for changing a custom domain association.
module Amazonka.Redshift.ModifyCustomDomainAssociation
  ( -- * Creating a Request
    ModifyCustomDomainAssociation (..),
    newModifyCustomDomainAssociation,

    -- * Request Lenses
    modifyCustomDomainAssociation_customDomainCertificateArn,
    modifyCustomDomainAssociation_customDomainName,
    modifyCustomDomainAssociation_clusterIdentifier,

    -- * Destructuring the Response
    ModifyCustomDomainAssociationResponse (..),
    newModifyCustomDomainAssociationResponse,

    -- * Response Lenses
    modifyCustomDomainAssociationResponse_clusterIdentifier,
    modifyCustomDomainAssociationResponse_customDomainCertExpiryTime,
    modifyCustomDomainAssociationResponse_customDomainCertificateArn,
    modifyCustomDomainAssociationResponse_customDomainName,
    modifyCustomDomainAssociationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyCustomDomainAssociation' smart constructor.
data ModifyCustomDomainAssociation = ModifyCustomDomainAssociation'
  { -- | The certificate Amazon Resource Name (ARN) for the changed custom domain
    -- association.
    customDomainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name for a changed custom domain association.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the cluster to change a custom domain association for.
    clusterIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCustomDomainAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customDomainCertificateArn', 'modifyCustomDomainAssociation_customDomainCertificateArn' - The certificate Amazon Resource Name (ARN) for the changed custom domain
-- association.
--
-- 'customDomainName', 'modifyCustomDomainAssociation_customDomainName' - The custom domain name for a changed custom domain association.
--
-- 'clusterIdentifier', 'modifyCustomDomainAssociation_clusterIdentifier' - The identifier of the cluster to change a custom domain association for.
newModifyCustomDomainAssociation ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  ModifyCustomDomainAssociation
newModifyCustomDomainAssociation pClusterIdentifier_ =
  ModifyCustomDomainAssociation'
    { customDomainCertificateArn =
        Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      clusterIdentifier = pClusterIdentifier_
    }

-- | The certificate Amazon Resource Name (ARN) for the changed custom domain
-- association.
modifyCustomDomainAssociation_customDomainCertificateArn :: Lens.Lens' ModifyCustomDomainAssociation (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociation_customDomainCertificateArn = Lens.lens (\ModifyCustomDomainAssociation' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@ModifyCustomDomainAssociation' {} a -> s {customDomainCertificateArn = a} :: ModifyCustomDomainAssociation)

-- | The custom domain name for a changed custom domain association.
modifyCustomDomainAssociation_customDomainName :: Lens.Lens' ModifyCustomDomainAssociation (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociation_customDomainName = Lens.lens (\ModifyCustomDomainAssociation' {customDomainName} -> customDomainName) (\s@ModifyCustomDomainAssociation' {} a -> s {customDomainName = a} :: ModifyCustomDomainAssociation)

-- | The identifier of the cluster to change a custom domain association for.
modifyCustomDomainAssociation_clusterIdentifier :: Lens.Lens' ModifyCustomDomainAssociation Prelude.Text
modifyCustomDomainAssociation_clusterIdentifier = Lens.lens (\ModifyCustomDomainAssociation' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyCustomDomainAssociation' {} a -> s {clusterIdentifier = a} :: ModifyCustomDomainAssociation)

instance
  Core.AWSRequest
    ModifyCustomDomainAssociation
  where
  type
    AWSResponse ModifyCustomDomainAssociation =
      ModifyCustomDomainAssociationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyCustomDomainAssociationResult"
      ( \s h x ->
          ModifyCustomDomainAssociationResponse'
            Prelude.<$> (x Data..@? "ClusterIdentifier")
            Prelude.<*> (x Data..@? "CustomDomainCertExpiryTime")
            Prelude.<*> (x Data..@? "CustomDomainCertificateArn")
            Prelude.<*> (x Data..@? "CustomDomainName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyCustomDomainAssociation
  where
  hashWithSalt _salt ModifyCustomDomainAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` customDomainCertificateArn
      `Prelude.hashWithSalt` customDomainName
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData ModifyCustomDomainAssociation where
  rnf ModifyCustomDomainAssociation' {..} =
    Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf clusterIdentifier

instance Data.ToHeaders ModifyCustomDomainAssociation where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyCustomDomainAssociation where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyCustomDomainAssociation where
  toQuery ModifyCustomDomainAssociation' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyCustomDomainAssociation" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "CustomDomainCertificateArn"
          Data.=: customDomainCertificateArn,
        "CustomDomainName" Data.=: customDomainName,
        "ClusterIdentifier" Data.=: clusterIdentifier
      ]

-- | /See:/ 'newModifyCustomDomainAssociationResponse' smart constructor.
data ModifyCustomDomainAssociationResponse = ModifyCustomDomainAssociationResponse'
  { -- | The identifier of the cluster associated with the result for the changed
    -- custom domain association.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The certificate expiration time associated with the result for the
    -- changed custom domain association.
    customDomainCertExpiryTime :: Prelude.Maybe Prelude.Text,
    -- | The certificate Amazon Resource Name (ARN) associated with the result
    -- for the changed custom domain association.
    customDomainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name associated with the result for the changed custom
    -- domain association.
    customDomainName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyCustomDomainAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'modifyCustomDomainAssociationResponse_clusterIdentifier' - The identifier of the cluster associated with the result for the changed
-- custom domain association.
--
-- 'customDomainCertExpiryTime', 'modifyCustomDomainAssociationResponse_customDomainCertExpiryTime' - The certificate expiration time associated with the result for the
-- changed custom domain association.
--
-- 'customDomainCertificateArn', 'modifyCustomDomainAssociationResponse_customDomainCertificateArn' - The certificate Amazon Resource Name (ARN) associated with the result
-- for the changed custom domain association.
--
-- 'customDomainName', 'modifyCustomDomainAssociationResponse_customDomainName' - The custom domain name associated with the result for the changed custom
-- domain association.
--
-- 'httpStatus', 'modifyCustomDomainAssociationResponse_httpStatus' - The response's http status code.
newModifyCustomDomainAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyCustomDomainAssociationResponse
newModifyCustomDomainAssociationResponse pHttpStatus_ =
  ModifyCustomDomainAssociationResponse'
    { clusterIdentifier =
        Prelude.Nothing,
      customDomainCertExpiryTime =
        Prelude.Nothing,
      customDomainCertificateArn =
        Prelude.Nothing,
      customDomainName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the cluster associated with the result for the changed
-- custom domain association.
modifyCustomDomainAssociationResponse_clusterIdentifier :: Lens.Lens' ModifyCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociationResponse_clusterIdentifier = Lens.lens (\ModifyCustomDomainAssociationResponse' {clusterIdentifier} -> clusterIdentifier) (\s@ModifyCustomDomainAssociationResponse' {} a -> s {clusterIdentifier = a} :: ModifyCustomDomainAssociationResponse)

-- | The certificate expiration time associated with the result for the
-- changed custom domain association.
modifyCustomDomainAssociationResponse_customDomainCertExpiryTime :: Lens.Lens' ModifyCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociationResponse_customDomainCertExpiryTime = Lens.lens (\ModifyCustomDomainAssociationResponse' {customDomainCertExpiryTime} -> customDomainCertExpiryTime) (\s@ModifyCustomDomainAssociationResponse' {} a -> s {customDomainCertExpiryTime = a} :: ModifyCustomDomainAssociationResponse)

-- | The certificate Amazon Resource Name (ARN) associated with the result
-- for the changed custom domain association.
modifyCustomDomainAssociationResponse_customDomainCertificateArn :: Lens.Lens' ModifyCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociationResponse_customDomainCertificateArn = Lens.lens (\ModifyCustomDomainAssociationResponse' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@ModifyCustomDomainAssociationResponse' {} a -> s {customDomainCertificateArn = a} :: ModifyCustomDomainAssociationResponse)

-- | The custom domain name associated with the result for the changed custom
-- domain association.
modifyCustomDomainAssociationResponse_customDomainName :: Lens.Lens' ModifyCustomDomainAssociationResponse (Prelude.Maybe Prelude.Text)
modifyCustomDomainAssociationResponse_customDomainName = Lens.lens (\ModifyCustomDomainAssociationResponse' {customDomainName} -> customDomainName) (\s@ModifyCustomDomainAssociationResponse' {} a -> s {customDomainName = a} :: ModifyCustomDomainAssociationResponse)

-- | The response's http status code.
modifyCustomDomainAssociationResponse_httpStatus :: Lens.Lens' ModifyCustomDomainAssociationResponse Prelude.Int
modifyCustomDomainAssociationResponse_httpStatus = Lens.lens (\ModifyCustomDomainAssociationResponse' {httpStatus} -> httpStatus) (\s@ModifyCustomDomainAssociationResponse' {} a -> s {httpStatus = a} :: ModifyCustomDomainAssociationResponse)

instance
  Prelude.NFData
    ModifyCustomDomainAssociationResponse
  where
  rnf ModifyCustomDomainAssociationResponse' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf customDomainCertExpiryTime
      `Prelude.seq` Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf customDomainName
      `Prelude.seq` Prelude.rnf httpStatus
