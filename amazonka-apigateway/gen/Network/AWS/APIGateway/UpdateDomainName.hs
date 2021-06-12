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
-- Module      : Network.AWS.APIGateway.UpdateDomainName
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes information about the DomainName resource.
module Network.AWS.APIGateway.UpdateDomainName
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
    domainName_regionalHostedZoneId,
    domainName_regionalCertificateName,
    domainName_mutualTlsAuthentication,
    domainName_endpointConfiguration,
    domainName_distributionHostedZoneId,
    domainName_certificateArn,
    domainName_domainNameStatusMessage,
    domainName_distributionDomainName,
    domainName_certificateUploadDate,
    domainName_domainName,
    domainName_tags,
    domainName_securityPolicy,
    domainName_domainNameStatus,
    domainName_regionalCertificateArn,
    domainName_certificateName,
    domainName_regionalDomainName,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to change information about the DomainName resource.
--
-- /See:/ 'newUpdateDomainName' smart constructor.
data UpdateDomainName = UpdateDomainName'
  { -- | A list of update operations to be applied to the specified resource and
    -- in the order specified in this list.
    patchOperations :: Core.Maybe [PatchOperation],
    -- | [Required] The name of the DomainName resource to be changed.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDomainName' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'patchOperations', 'updateDomainName_patchOperations' - A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
--
-- 'domainName', 'updateDomainName_domainName' - [Required] The name of the DomainName resource to be changed.
newUpdateDomainName ::
  -- | 'domainName'
  Core.Text ->
  UpdateDomainName
newUpdateDomainName pDomainName_ =
  UpdateDomainName'
    { patchOperations = Core.Nothing,
      domainName = pDomainName_
    }

-- | A list of update operations to be applied to the specified resource and
-- in the order specified in this list.
updateDomainName_patchOperations :: Lens.Lens' UpdateDomainName (Core.Maybe [PatchOperation])
updateDomainName_patchOperations = Lens.lens (\UpdateDomainName' {patchOperations} -> patchOperations) (\s@UpdateDomainName' {} a -> s {patchOperations = a} :: UpdateDomainName) Core.. Lens.mapping Lens._Coerce

-- | [Required] The name of the DomainName resource to be changed.
updateDomainName_domainName :: Lens.Lens' UpdateDomainName Core.Text
updateDomainName_domainName = Lens.lens (\UpdateDomainName' {domainName} -> domainName) (\s@UpdateDomainName' {} a -> s {domainName = a} :: UpdateDomainName)

instance Core.AWSRequest UpdateDomainName where
  type AWSResponse UpdateDomainName = DomainName
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable UpdateDomainName

instance Core.NFData UpdateDomainName

instance Core.ToHeaders UpdateDomainName where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateDomainName where
  toJSON UpdateDomainName' {..} =
    Core.object
      ( Core.catMaybes
          [ ("patchOperations" Core..=)
              Core.<$> patchOperations
          ]
      )

instance Core.ToPath UpdateDomainName where
  toPath UpdateDomainName' {..} =
    Core.mconcat
      ["/domainnames/", Core.toBS domainName]

instance Core.ToQuery UpdateDomainName where
  toQuery = Core.const Core.mempty
