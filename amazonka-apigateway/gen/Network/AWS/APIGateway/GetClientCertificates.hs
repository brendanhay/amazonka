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
-- Module      : Network.AWS.APIGateway.GetClientCertificates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a collection of ClientCertificate resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetClientCertificates
  ( -- * Creating a Request
    GetClientCertificates (..),
    newGetClientCertificates,

    -- * Request Lenses
    getClientCertificates_position,
    getClientCertificates_limit,

    -- * Destructuring the Response
    GetClientCertificatesResponse (..),
    newGetClientCertificatesResponse,

    -- * Response Lenses
    getClientCertificatesResponse_items,
    getClientCertificatesResponse_position,
    getClientCertificatesResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about a collection of ClientCertificate
-- resources.
--
-- /See:/ 'newGetClientCertificates' smart constructor.
data GetClientCertificates = GetClientCertificates'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClientCertificates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getClientCertificates_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getClientCertificates_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetClientCertificates ::
  GetClientCertificates
newGetClientCertificates =
  GetClientCertificates'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The current pagination position in the paged result set.
getClientCertificates_position :: Lens.Lens' GetClientCertificates (Prelude.Maybe Prelude.Text)
getClientCertificates_position = Lens.lens (\GetClientCertificates' {position} -> position) (\s@GetClientCertificates' {} a -> s {position = a} :: GetClientCertificates)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getClientCertificates_limit :: Lens.Lens' GetClientCertificates (Prelude.Maybe Prelude.Int)
getClientCertificates_limit = Lens.lens (\GetClientCertificates' {limit} -> limit) (\s@GetClientCertificates' {} a -> s {limit = a} :: GetClientCertificates)

instance Core.AWSPager GetClientCertificates where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getClientCertificatesResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getClientCertificatesResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getClientCertificates_position
          Lens..~ rs
          Lens.^? getClientCertificatesResponse_position
            Prelude.. Lens._Just

instance Core.AWSRequest GetClientCertificates where
  type
    AWSResponse GetClientCertificates =
      GetClientCertificatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetClientCertificatesResponse'
            Prelude.<$> (x Core..?> "item" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetClientCertificates

instance Prelude.NFData GetClientCertificates

instance Core.ToHeaders GetClientCertificates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Core.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Core.ToPath GetClientCertificates where
  toPath = Prelude.const "/clientcertificates"

instance Core.ToQuery GetClientCertificates where
  toQuery GetClientCertificates' {..} =
    Prelude.mconcat
      ["position" Core.=: position, "limit" Core.=: limit]

-- | Represents a collection of ClientCertificate resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/getting-started-client-side-ssl-authentication.html Use Client-Side Certificate>
--
-- /See:/ 'newGetClientCertificatesResponse' smart constructor.
data GetClientCertificatesResponse = GetClientCertificatesResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [ClientCertificate],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetClientCertificatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getClientCertificatesResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getClientCertificatesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getClientCertificatesResponse_httpStatus' - The response's http status code.
newGetClientCertificatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetClientCertificatesResponse
newGetClientCertificatesResponse pHttpStatus_ =
  GetClientCertificatesResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getClientCertificatesResponse_items :: Lens.Lens' GetClientCertificatesResponse (Prelude.Maybe [ClientCertificate])
getClientCertificatesResponse_items = Lens.lens (\GetClientCertificatesResponse' {items} -> items) (\s@GetClientCertificatesResponse' {} a -> s {items = a} :: GetClientCertificatesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
getClientCertificatesResponse_position :: Lens.Lens' GetClientCertificatesResponse (Prelude.Maybe Prelude.Text)
getClientCertificatesResponse_position = Lens.lens (\GetClientCertificatesResponse' {position} -> position) (\s@GetClientCertificatesResponse' {} a -> s {position = a} :: GetClientCertificatesResponse)

-- | The response's http status code.
getClientCertificatesResponse_httpStatus :: Lens.Lens' GetClientCertificatesResponse Prelude.Int
getClientCertificatesResponse_httpStatus = Lens.lens (\GetClientCertificatesResponse' {httpStatus} -> httpStatus) (\s@GetClientCertificatesResponse' {} a -> s {httpStatus = a} :: GetClientCertificatesResponse)

instance Prelude.NFData GetClientCertificatesResponse
