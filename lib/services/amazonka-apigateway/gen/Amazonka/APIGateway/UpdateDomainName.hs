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
-- Module      : Amazonka.APIGateway.UpdateDomainName
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the DomainName resource.
module Amazonka.APIGateway.UpdateDomainName
  ( -- * Creating a Request
    UpdateDomainName (..),
    newUpdateDomainName,

    -- * Request Lenses
    updateDomainName_patchOperations,
    updateDomainName_domainName,

    -- * Destructuring the Response
    DomainName (..),
    newDomainName,

    -- * Response Lenses
    domainName_tags,
    domainName_domainNameStatus,
    domainName_mutualTlsAuthentication,
    domainName_regionalCertificateName,
    domainName_regionalDomainName,
    domainName_domainName,
    domainName_ownershipVerificationCertificateArn,
    domainName_regionalCertificateArn,
    domainName_regionalHostedZoneId,
    domainName_certificateName,
    domainName_domainNameStatusMessage,
    domainName_certificateArn,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_securityPolicy,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to change information about the DomainName resource.
--
-- /See:/ 'newUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { -- | For more information about supported patch operations, see
    -- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
    patchOperations :: Prelude.Maybe [PatchOperation],
    -- | The name of the DomainName resource to be changed.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateDomainName_patchOperations' - For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
--
-- 'domainName', 'updateDomainName_domainName' - The name of the DomainName resource to be changed.
newUpdateDomainName ::
  -- | 'domainName'
  Prelude.Text ->
  UpdateDomainName
newUpdateDomainName pDomainName_ =
  UpdateDomainName'
    { patchOperations =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | For more information about supported patch operations, see
-- <https://docs.aws.amazon.com/apigateway/latest/api/patch-operations.html Patch Operations>.
updateDomainName_patchOperations :: Lens.Lens' UpdateDomainName (Prelude.Maybe [PatchOperation])
updateDomainName_patchOperations = Lens.lens (\UpdateDomainName' {patchOperations} -> patchOperations) (\s@UpdateDomainName' {} a -> s {patchOperations = a} :: UpdateDomainName) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DomainName resource to be changed.
updateDomainName_domainName :: Lens.Lens' UpdateDomainName Prelude.Text
updateDomainName_domainName = Lens.lens (\UpdateDomainName' {domainName} -> domainName) (\s@UpdateDomainName' {} a -> s {domainName = a} :: UpdateDomainName)

instance Core.AWSRequest UpdateDomainName where
  type AWSResponse UpdateDomainName = DomainName
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable UpdateDomainName where
  hashWithSalt _salt UpdateDomainName' {..} =
    _salt `Prelude.hashWithSalt` patchOperations
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData UpdateDomainName where
  rnf UpdateDomainName' {..} =
    Prelude.rnf patchOperations
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders UpdateDomainName where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON UpdateDomainName where
  toJSON UpdateDomainName' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("patchOperations" Data..=)
              Prelude.<$> patchOperations
          ]
      )

instance Data.ToPath UpdateDomainName where
  toPath UpdateDomainName' {..} =
    Prelude.mconcat
      ["/domainnames/", Data.toBS domainName]

instance Data.ToQuery UpdateDomainName where
  toQuery = Prelude.const Prelude.mempty
