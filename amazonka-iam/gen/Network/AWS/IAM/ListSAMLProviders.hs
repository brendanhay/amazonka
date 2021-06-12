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
-- Module      : Network.AWS.IAM.ListSAMLProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the SAML provider resource objects defined in IAM in the account.
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for a SAML provider, see GetSAMLProvider.
--
-- This operation requires
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
module Network.AWS.IAM.ListSAMLProviders
  ( -- * Creating a Request
    ListSAMLProviders (..),
    newListSAMLProviders,

    -- * Destructuring the Response
    ListSAMLProvidersResponse (..),
    newListSAMLProvidersResponse,

    -- * Response Lenses
    listSAMLProvidersResponse_sAMLProviderList,
    listSAMLProvidersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSAMLProviders' smart constructor.
data ListSAMLProviders = ListSAMLProviders'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSAMLProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListSAMLProviders ::
  ListSAMLProviders
newListSAMLProviders = ListSAMLProviders'

instance Core.AWSRequest ListSAMLProviders where
  type
    AWSResponse ListSAMLProviders =
      ListSAMLProvidersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListSAMLProvidersResult"
      ( \s h x ->
          ListSAMLProvidersResponse'
            Core.<$> ( x Core..@? "SAMLProviderList" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListSAMLProviders

instance Core.NFData ListSAMLProviders

instance Core.ToHeaders ListSAMLProviders where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListSAMLProviders where
  toPath = Core.const "/"

instance Core.ToQuery ListSAMLProviders where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("ListSAMLProviders" :: Core.ByteString),
            "Version" Core.=: ("2010-05-08" :: Core.ByteString)
          ]
      )

-- | Contains the response to a successful ListSAMLProviders request.
--
-- /See:/ 'newListSAMLProvidersResponse' smart constructor.
data ListSAMLProvidersResponse = ListSAMLProvidersResponse'
  { -- | The list of SAML provider resource objects defined in IAM for this AWS
    -- account.
    sAMLProviderList :: Core.Maybe [SAMLProviderListEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListSAMLProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderList', 'listSAMLProvidersResponse_sAMLProviderList' - The list of SAML provider resource objects defined in IAM for this AWS
-- account.
--
-- 'httpStatus', 'listSAMLProvidersResponse_httpStatus' - The response's http status code.
newListSAMLProvidersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListSAMLProvidersResponse
newListSAMLProvidersResponse pHttpStatus_ =
  ListSAMLProvidersResponse'
    { sAMLProviderList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of SAML provider resource objects defined in IAM for this AWS
-- account.
listSAMLProvidersResponse_sAMLProviderList :: Lens.Lens' ListSAMLProvidersResponse (Core.Maybe [SAMLProviderListEntry])
listSAMLProvidersResponse_sAMLProviderList = Lens.lens (\ListSAMLProvidersResponse' {sAMLProviderList} -> sAMLProviderList) (\s@ListSAMLProvidersResponse' {} a -> s {sAMLProviderList = a} :: ListSAMLProvidersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSAMLProvidersResponse_httpStatus :: Lens.Lens' ListSAMLProvidersResponse Core.Int
listSAMLProvidersResponse_httpStatus = Lens.lens (\ListSAMLProvidersResponse' {httpStatus} -> httpStatus) (\s@ListSAMLProvidersResponse' {} a -> s {httpStatus = a} :: ListSAMLProvidersResponse)

instance Core.NFData ListSAMLProvidersResponse
