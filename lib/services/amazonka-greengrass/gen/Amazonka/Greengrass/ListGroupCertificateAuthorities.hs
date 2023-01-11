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
-- Module      : Amazonka.Greengrass.ListGroupCertificateAuthorities
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current CAs for a group.
module Amazonka.Greengrass.ListGroupCertificateAuthorities
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListGroupCertificateAuthorities' smart constructor.
data ListGroupCertificateAuthorities = ListGroupCertificateAuthorities'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListGroupCertificateAuthorities
newListGroupCertificateAuthorities pGroupId_ =
  ListGroupCertificateAuthorities'
    { groupId =
        pGroupId_
    }

-- | The ID of the Greengrass group.
listGroupCertificateAuthorities_groupId :: Lens.Lens' ListGroupCertificateAuthorities Prelude.Text
listGroupCertificateAuthorities_groupId = Lens.lens (\ListGroupCertificateAuthorities' {groupId} -> groupId) (\s@ListGroupCertificateAuthorities' {} a -> s {groupId = a} :: ListGroupCertificateAuthorities)

instance
  Core.AWSRequest
    ListGroupCertificateAuthorities
  where
  type
    AWSResponse ListGroupCertificateAuthorities =
      ListGroupCertificateAuthoritiesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGroupCertificateAuthoritiesResponse'
            Prelude.<$> ( x Data..?> "GroupCertificateAuthorities"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListGroupCertificateAuthorities
  where
  hashWithSalt
    _salt
    ListGroupCertificateAuthorities' {..} =
      _salt `Prelude.hashWithSalt` groupId

instance
  Prelude.NFData
    ListGroupCertificateAuthorities
  where
  rnf ListGroupCertificateAuthorities' {..} =
    Prelude.rnf groupId

instance
  Data.ToHeaders
    ListGroupCertificateAuthorities
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListGroupCertificateAuthorities where
  toPath ListGroupCertificateAuthorities' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/certificateauthorities"
      ]

instance Data.ToQuery ListGroupCertificateAuthorities where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListGroupCertificateAuthoritiesResponse' smart constructor.
data ListGroupCertificateAuthoritiesResponse = ListGroupCertificateAuthoritiesResponse'
  { -- | A list of certificate authorities associated with the group.
    groupCertificateAuthorities :: Prelude.Maybe [GroupCertificateAuthorityProperties],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListGroupCertificateAuthoritiesResponse
newListGroupCertificateAuthoritiesResponse
  pHttpStatus_ =
    ListGroupCertificateAuthoritiesResponse'
      { groupCertificateAuthorities =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | A list of certificate authorities associated with the group.
listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities :: Lens.Lens' ListGroupCertificateAuthoritiesResponse (Prelude.Maybe [GroupCertificateAuthorityProperties])
listGroupCertificateAuthoritiesResponse_groupCertificateAuthorities = Lens.lens (\ListGroupCertificateAuthoritiesResponse' {groupCertificateAuthorities} -> groupCertificateAuthorities) (\s@ListGroupCertificateAuthoritiesResponse' {} a -> s {groupCertificateAuthorities = a} :: ListGroupCertificateAuthoritiesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listGroupCertificateAuthoritiesResponse_httpStatus :: Lens.Lens' ListGroupCertificateAuthoritiesResponse Prelude.Int
listGroupCertificateAuthoritiesResponse_httpStatus = Lens.lens (\ListGroupCertificateAuthoritiesResponse' {httpStatus} -> httpStatus) (\s@ListGroupCertificateAuthoritiesResponse' {} a -> s {httpStatus = a} :: ListGroupCertificateAuthoritiesResponse)

instance
  Prelude.NFData
    ListGroupCertificateAuthoritiesResponse
  where
  rnf ListGroupCertificateAuthoritiesResponse' {..} =
    Prelude.rnf groupCertificateAuthorities
      `Prelude.seq` Prelude.rnf httpStatus
