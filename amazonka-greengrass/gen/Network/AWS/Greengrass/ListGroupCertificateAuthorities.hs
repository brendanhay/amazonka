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
-- Module      : Network.AWS.Greengrass.ListGroupCertificateAuthorities
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current CAs for a group.
module Network.AWS.Greengrass.ListGroupCertificateAuthorities
  ( -- * Creating a Request
    ListGroupCertificateAuthorities (..),
    newListGroupCertificateAuthorities,

    -- * Request Lenses
    listGroupCertificateAuthorities_groupId,

    -- * Destructuring the Response
    ListGroupCertificateAuthoritiesResponse (..),
    newListGroupCertificateAuthoritiesResponse,

    -- * Response Lenses
    listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities,
    listGroupCertificateAuthoritiesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListGroupCertificateAuthorities' smart constructor.
data ListGroupCertificateAuthorities = ListGroupCertificateAuthorities'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroupCertificateAuthorities' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'listGroupCertificateAuthorities_groupId' - The ID of the Greengrass group.
newListGroupCertificateAuthorities ::
  -- | 'groupId'
  Core.Text ->
  ListGroupCertificateAuthorities
newListGroupCertificateAuthorities pGroupId_ =
  ListGroupCertificateAuthorities'
    { groupId =
        pGroupId_
    }

-- | The ID of the Greengrass group.
listGroupCertificateAuthorities_groupId :: Lens.Lens' ListGroupCertificateAuthorities Core.Text
listGroupCertificateAuthorities_groupId = Lens.lens (\ListGroupCertificateAuthorities' {groupId} -> groupId) (\s@ListGroupCertificateAuthorities' {} a -> s {groupId = a} :: ListGroupCertificateAuthorities)

instance
  Core.AWSRequest
    ListGroupCertificateAuthorities
  where
  type
    AWSResponse ListGroupCertificateAuthorities =
      ListGroupCertificateAuthoritiesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupCertificateAuthoritiesResponse'
            Core.<$> ( x Core..?> "GroupCertificateAuthorities"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListGroupCertificateAuthorities

instance Core.NFData ListGroupCertificateAuthorities

instance
  Core.ToHeaders
    ListGroupCertificateAuthorities
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListGroupCertificateAuthorities where
  toPath ListGroupCertificateAuthorities' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/certificateauthorities"
      ]

instance Core.ToQuery ListGroupCertificateAuthorities where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListGroupCertificateAuthoritiesResponse' smart constructor.
data ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse'
  { -- | A list of certificate authorities associated with the group.
    groupCertificateAuthorities :: Core.Maybe [GroupCertificateAuthorityProperties],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListGroupCertificateAuthoritiesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupCertificateAuthorities', 'listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities' - A list of certificate authorities associated with the group.
--
-- 'httpStatus', 'listGroupCertificateAuthoritiesResponse_httpStatus' - The response's http status code.
newListGroupCertificateAuthoritiesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListGroupCertificateAuthoritiesResponse
newListGroupCertificateAuthoritiesResponse
  pHttpStatus_ =
    ListGroupCertificateAuthoritiesResponse'
      { groupCertificateAuthorities =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of certificate authorities associated with the group.
listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities :: Lens.Lens' ListGroupCertificateAuthoritiesResponse (Core.Maybe [GroupCertificateAuthorityProperties])
listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities = Lens.lens (\ListGroupCertificateAuthoritiesResponse' {groupCertificateAuthorities} -> groupCertificateAuthorities) (\s@ListGroupCertificateAuthoritiesResponse' {} a -> s {groupCertificateAuthorities = a} :: ListGroupCertificateAuthoritiesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listGroupCertificateAuthoritiesResponse_httpStatus :: Lens.Lens' ListGroupCertificateAuthoritiesResponse Core.Int
listGroupCertificateAuthoritiesResponse_httpStatus = Lens.lens (\ListGroupCertificateAuthoritiesResponse' {httpStatus} -> httpStatus) (\s@ListGroupCertificateAuthoritiesResponse' {} a -> s {httpStatus = a} :: ListGroupCertificateAuthoritiesResponse)

instance
  Core.NFData
    ListGroupCertificateAuthoritiesResponse
