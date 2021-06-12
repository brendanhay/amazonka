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
-- Module      : Network.AWS.IAM.ListOpenIDConnectProviders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about the IAM OpenID Connect (OIDC) provider resource
-- objects defined in the AWS account.
--
-- IAM resource-listing operations return a subset of the available
-- attributes for the resource. For example, this operation does not return
-- tags, even though they are an attribute of the returned object. To view
-- all of the information for an OIDC provider, see
-- GetOpenIDConnectProvider.
module Network.AWS.IAM.ListOpenIDConnectProviders
  ( -- * Creating a Request
    ListOpenIDConnectProviders (..),
    newListOpenIDConnectProviders,

    -- * Destructuring the Response
    ListOpenIDConnectProvidersResponse (..),
    newListOpenIDConnectProvidersResponse,

    -- * Response Lenses
    listOpenIDConnectProvidersResponse_openIDConnectProviderList,
    listOpenIDConnectProvidersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOpenIDConnectProviders' smart constructor.
data ListOpenIDConnectProviders = ListOpenIDConnectProviders'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProviders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newListOpenIDConnectProviders ::
  ListOpenIDConnectProviders
newListOpenIDConnectProviders =
  ListOpenIDConnectProviders'

instance Core.AWSRequest ListOpenIDConnectProviders where
  type
    AWSResponse ListOpenIDConnectProviders =
      ListOpenIDConnectProvidersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ListOpenIDConnectProvidersResult"
      ( \s h x ->
          ListOpenIDConnectProvidersResponse'
            Core.<$> ( x Core..@? "OpenIDConnectProviderList"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOpenIDConnectProviders

instance Core.NFData ListOpenIDConnectProviders

instance Core.ToHeaders ListOpenIDConnectProviders where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ListOpenIDConnectProviders where
  toPath = Core.const "/"

instance Core.ToQuery ListOpenIDConnectProviders where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ("ListOpenIDConnectProviders" :: Core.ByteString),
            "Version" Core.=: ("2010-05-08" :: Core.ByteString)
          ]
      )

-- | Contains the response to a successful ListOpenIDConnectProviders
-- request.
--
-- /See:/ 'newListOpenIDConnectProvidersResponse' smart constructor.
data ListOpenIDConnectProvidersResponse = ListOpenIDConnectProvidersResponse'
  { -- | The list of IAM OIDC provider resource objects defined in the AWS
    -- account.
    openIDConnectProviderList :: Core.Maybe [OpenIDConnectProviderListEntry],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOpenIDConnectProvidersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'openIDConnectProviderList', 'listOpenIDConnectProvidersResponse_openIDConnectProviderList' - The list of IAM OIDC provider resource objects defined in the AWS
-- account.
--
-- 'httpStatus', 'listOpenIDConnectProvidersResponse_httpStatus' - The response's http status code.
newListOpenIDConnectProvidersResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOpenIDConnectProvidersResponse
newListOpenIDConnectProvidersResponse pHttpStatus_ =
  ListOpenIDConnectProvidersResponse'
    { openIDConnectProviderList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of IAM OIDC provider resource objects defined in the AWS
-- account.
listOpenIDConnectProvidersResponse_openIDConnectProviderList :: Lens.Lens' ListOpenIDConnectProvidersResponse (Core.Maybe [OpenIDConnectProviderListEntry])
listOpenIDConnectProvidersResponse_openIDConnectProviderList = Lens.lens (\ListOpenIDConnectProvidersResponse' {openIDConnectProviderList} -> openIDConnectProviderList) (\s@ListOpenIDConnectProvidersResponse' {} a -> s {openIDConnectProviderList = a} :: ListOpenIDConnectProvidersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listOpenIDConnectProvidersResponse_httpStatus :: Lens.Lens' ListOpenIDConnectProvidersResponse Core.Int
listOpenIDConnectProvidersResponse_httpStatus = Lens.lens (\ListOpenIDConnectProvidersResponse' {httpStatus} -> httpStatus) (\s@ListOpenIDConnectProvidersResponse' {} a -> s {httpStatus = a} :: ListOpenIDConnectProvidersResponse)

instance
  Core.NFData
    ListOpenIDConnectProvidersResponse
