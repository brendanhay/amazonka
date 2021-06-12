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
-- Module      : Network.AWS.CloudSearch.UpdateServiceAccessPolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the access rules that control access to the domain\'s
-- document and search endpoints. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-access.html Configuring Access for an Amazon CloudSearch Domain>.
module Network.AWS.CloudSearch.UpdateServiceAccessPolicies
  ( -- * Creating a Request
    UpdateServiceAccessPolicies (..),
    newUpdateServiceAccessPolicies,

    -- * Request Lenses
    updateServiceAccessPolicies_domainName,
    updateServiceAccessPolicies_accessPolicies,

    -- * Destructuring the Response
    UpdateServiceAccessPoliciesResponse (..),
    newUpdateServiceAccessPoliciesResponse,

    -- * Response Lenses
    updateServiceAccessPoliciesResponse_httpStatus,
    updateServiceAccessPoliciesResponse_accessPolicies,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @UpdateServiceAccessPolicies@
-- operation. Specifies the name of the domain you want to update and the
-- access rules you want to configure.
--
-- /See:/ 'newUpdateServiceAccessPolicies' smart constructor.
data UpdateServiceAccessPolicies = UpdateServiceAccessPolicies'
  { domainName :: Core.Text,
    -- | The access rules you want to configure. These rules replace any existing
    -- rules.
    accessPolicies :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServiceAccessPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'updateServiceAccessPolicies_domainName' - Undocumented member.
--
-- 'accessPolicies', 'updateServiceAccessPolicies_accessPolicies' - The access rules you want to configure. These rules replace any existing
-- rules.
newUpdateServiceAccessPolicies ::
  -- | 'domainName'
  Core.Text ->
  -- | 'accessPolicies'
  Core.Text ->
  UpdateServiceAccessPolicies
newUpdateServiceAccessPolicies
  pDomainName_
  pAccessPolicies_ =
    UpdateServiceAccessPolicies'
      { domainName =
          pDomainName_,
        accessPolicies = pAccessPolicies_
      }

-- | Undocumented member.
updateServiceAccessPolicies_domainName :: Lens.Lens' UpdateServiceAccessPolicies Core.Text
updateServiceAccessPolicies_domainName = Lens.lens (\UpdateServiceAccessPolicies' {domainName} -> domainName) (\s@UpdateServiceAccessPolicies' {} a -> s {domainName = a} :: UpdateServiceAccessPolicies)

-- | The access rules you want to configure. These rules replace any existing
-- rules.
updateServiceAccessPolicies_accessPolicies :: Lens.Lens' UpdateServiceAccessPolicies Core.Text
updateServiceAccessPolicies_accessPolicies = Lens.lens (\UpdateServiceAccessPolicies' {accessPolicies} -> accessPolicies) (\s@UpdateServiceAccessPolicies' {} a -> s {accessPolicies = a} :: UpdateServiceAccessPolicies)

instance Core.AWSRequest UpdateServiceAccessPolicies where
  type
    AWSResponse UpdateServiceAccessPolicies =
      UpdateServiceAccessPoliciesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "UpdateServiceAccessPoliciesResult"
      ( \s h x ->
          UpdateServiceAccessPoliciesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "AccessPolicies")
      )

instance Core.Hashable UpdateServiceAccessPolicies

instance Core.NFData UpdateServiceAccessPolicies

instance Core.ToHeaders UpdateServiceAccessPolicies where
  toHeaders = Core.const Core.mempty

instance Core.ToPath UpdateServiceAccessPolicies where
  toPath = Core.const "/"

instance Core.ToQuery UpdateServiceAccessPolicies where
  toQuery UpdateServiceAccessPolicies' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("UpdateServiceAccessPolicies" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "DomainName" Core.=: domainName,
        "AccessPolicies" Core.=: accessPolicies
      ]

-- | The result of an @UpdateServiceAccessPolicies@ request. Contains the new
-- access policies.
--
-- /See:/ 'newUpdateServiceAccessPoliciesResponse' smart constructor.
data UpdateServiceAccessPoliciesResponse = UpdateServiceAccessPoliciesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The access rules configured for the domain.
    accessPolicies :: AccessPoliciesStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateServiceAccessPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateServiceAccessPoliciesResponse_httpStatus' - The response's http status code.
--
-- 'accessPolicies', 'updateServiceAccessPoliciesResponse_accessPolicies' - The access rules configured for the domain.
newUpdateServiceAccessPoliciesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'accessPolicies'
  AccessPoliciesStatus ->
  UpdateServiceAccessPoliciesResponse
newUpdateServiceAccessPoliciesResponse
  pHttpStatus_
  pAccessPolicies_ =
    UpdateServiceAccessPoliciesResponse'
      { httpStatus =
          pHttpStatus_,
        accessPolicies = pAccessPolicies_
      }

-- | The response's http status code.
updateServiceAccessPoliciesResponse_httpStatus :: Lens.Lens' UpdateServiceAccessPoliciesResponse Core.Int
updateServiceAccessPoliciesResponse_httpStatus = Lens.lens (\UpdateServiceAccessPoliciesResponse' {httpStatus} -> httpStatus) (\s@UpdateServiceAccessPoliciesResponse' {} a -> s {httpStatus = a} :: UpdateServiceAccessPoliciesResponse)

-- | The access rules configured for the domain.
updateServiceAccessPoliciesResponse_accessPolicies :: Lens.Lens' UpdateServiceAccessPoliciesResponse AccessPoliciesStatus
updateServiceAccessPoliciesResponse_accessPolicies = Lens.lens (\UpdateServiceAccessPoliciesResponse' {accessPolicies} -> accessPolicies) (\s@UpdateServiceAccessPoliciesResponse' {} a -> s {accessPolicies = a} :: UpdateServiceAccessPoliciesResponse)

instance
  Core.NFData
    UpdateServiceAccessPoliciesResponse
