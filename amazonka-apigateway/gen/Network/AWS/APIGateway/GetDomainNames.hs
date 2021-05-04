{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.APIGateway.GetDomainNames
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of DomainName resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetDomainNames
  ( -- * Creating a Request
    GetDomainNames (..),
    newGetDomainNames,

    -- * Request Lenses
    getDomainNames_position,
    getDomainNames_limit,

    -- * Destructuring the Response
    GetDomainNamesResponse (..),
    newGetDomainNamesResponse,

    -- * Response Lenses
    getDomainNamesResponse_items,
    getDomainNamesResponse_position,
    getDomainNamesResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to describe a collection of DomainName resources.
--
-- /See:/ 'newGetDomainNames' smart constructor.
data GetDomainNames = GetDomainNames'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNames' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getDomainNames_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getDomainNames_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
newGetDomainNames ::
  GetDomainNames
newGetDomainNames =
  GetDomainNames'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The current pagination position in the paged result set.
getDomainNames_position :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Text)
getDomainNames_position = Lens.lens (\GetDomainNames' {position} -> position) (\s@GetDomainNames' {} a -> s {position = a} :: GetDomainNames)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getDomainNames_limit :: Lens.Lens' GetDomainNames (Prelude.Maybe Prelude.Int)
getDomainNames_limit = Lens.lens (\GetDomainNames' {limit} -> limit) (\s@GetDomainNames' {} a -> s {limit = a} :: GetDomainNames)

instance Pager.AWSPager GetDomainNames where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getDomainNamesResponse_position Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getDomainNamesResponse_items Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getDomainNames_position
          Lens..~ rs
          Lens.^? getDomainNamesResponse_position Prelude.. Lens._Just

instance Prelude.AWSRequest GetDomainNames where
  type Rs GetDomainNames = GetDomainNamesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDomainNamesResponse'
            Prelude.<$> (x Prelude..?> "item" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDomainNames

instance Prelude.NFData GetDomainNames

instance Prelude.ToHeaders GetDomainNames where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetDomainNames where
  toPath = Prelude.const "/domainnames"

instance Prelude.ToQuery GetDomainNames where
  toQuery GetDomainNames' {..} =
    Prelude.mconcat
      [ "position" Prelude.=: position,
        "limit" Prelude.=: limit
      ]

-- | Represents a collection of DomainName resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Client-Side Certificate>
--
-- /See:/ 'newGetDomainNamesResponse' smart constructor.
data GetDomainNamesResponse = GetDomainNamesResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [DomainName],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetDomainNamesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getDomainNamesResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getDomainNamesResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getDomainNamesResponse_httpStatus' - The response's http status code.
newGetDomainNamesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDomainNamesResponse
newGetDomainNamesResponse pHttpStatus_ =
  GetDomainNamesResponse'
    { items = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getDomainNamesResponse_items :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe [DomainName])
getDomainNamesResponse_items = Lens.lens (\GetDomainNamesResponse' {items} -> items) (\s@GetDomainNamesResponse' {} a -> s {items = a} :: GetDomainNamesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
getDomainNamesResponse_position :: Lens.Lens' GetDomainNamesResponse (Prelude.Maybe Prelude.Text)
getDomainNamesResponse_position = Lens.lens (\GetDomainNamesResponse' {position} -> position) (\s@GetDomainNamesResponse' {} a -> s {position = a} :: GetDomainNamesResponse)

-- | The response's http status code.
getDomainNamesResponse_httpStatus :: Lens.Lens' GetDomainNamesResponse Prelude.Int
getDomainNamesResponse_httpStatus = Lens.lens (\GetDomainNamesResponse' {httpStatus} -> httpStatus) (\s@GetDomainNamesResponse' {} a -> s {httpStatus = a} :: GetDomainNamesResponse)

instance Prelude.NFData GetDomainNamesResponse
