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
-- Module      : Network.AWS.APIGateway.GetBasePathMappings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Represents a collection of BasePathMapping resources.
--
-- This operation returns paginated results.
module Network.AWS.APIGateway.GetBasePathMappings
  ( -- * Creating a Request
    GetBasePathMappings (..),
    newGetBasePathMappings,

    -- * Request Lenses
    getBasePathMappings_position,
    getBasePathMappings_limit,
    getBasePathMappings_domainName,

    -- * Destructuring the Response
    GetBasePathMappingsResponse (..),
    newGetBasePathMappingsResponse,

    -- * Response Lenses
    getBasePathMappingsResponse_items,
    getBasePathMappingsResponse_position,
    getBasePathMappingsResponse_httpStatus,
  )
where

import Network.AWS.APIGateway.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to get information about a collection of BasePathMapping
-- resources.
--
-- /See:/ 'newGetBasePathMappings' smart constructor.
data GetBasePathMappings = GetBasePathMappings'
  { -- | The current pagination position in the paged result set.
    position :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of returned results per page. The default value is 25
    -- and the maximum value is 500.
    limit :: Prelude.Maybe Prelude.Int,
    -- | [Required] The domain name of a BasePathMapping resource.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBasePathMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'position', 'getBasePathMappings_position' - The current pagination position in the paged result set.
--
-- 'limit', 'getBasePathMappings_limit' - The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
--
-- 'domainName', 'getBasePathMappings_domainName' - [Required] The domain name of a BasePathMapping resource.
newGetBasePathMappings ::
  -- | 'domainName'
  Prelude.Text ->
  GetBasePathMappings
newGetBasePathMappings pDomainName_ =
  GetBasePathMappings'
    { position = Prelude.Nothing,
      limit = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The current pagination position in the paged result set.
getBasePathMappings_position :: Lens.Lens' GetBasePathMappings (Prelude.Maybe Prelude.Text)
getBasePathMappings_position = Lens.lens (\GetBasePathMappings' {position} -> position) (\s@GetBasePathMappings' {} a -> s {position = a} :: GetBasePathMappings)

-- | The maximum number of returned results per page. The default value is 25
-- and the maximum value is 500.
getBasePathMappings_limit :: Lens.Lens' GetBasePathMappings (Prelude.Maybe Prelude.Int)
getBasePathMappings_limit = Lens.lens (\GetBasePathMappings' {limit} -> limit) (\s@GetBasePathMappings' {} a -> s {limit = a} :: GetBasePathMappings)

-- | [Required] The domain name of a BasePathMapping resource.
getBasePathMappings_domainName :: Lens.Lens' GetBasePathMappings Prelude.Text
getBasePathMappings_domainName = Lens.lens (\GetBasePathMappings' {domainName} -> domainName) (\s@GetBasePathMappings' {} a -> s {domainName = a} :: GetBasePathMappings)

instance Pager.AWSPager GetBasePathMappings where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getBasePathMappingsResponse_position
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getBasePathMappingsResponse_items
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getBasePathMappings_position
          Lens..~ rs
          Lens.^? getBasePathMappingsResponse_position
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetBasePathMappings where
  type
    Rs GetBasePathMappings =
      GetBasePathMappingsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBasePathMappingsResponse'
            Prelude.<$> (x Prelude..?> "item" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBasePathMappings

instance Prelude.NFData GetBasePathMappings

instance Prelude.ToHeaders GetBasePathMappings where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Prelude.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Prelude.ToPath GetBasePathMappings where
  toPath GetBasePathMappings' {..} =
    Prelude.mconcat
      [ "/domainnames/",
        Prelude.toBS domainName,
        "/basepathmappings"
      ]

instance Prelude.ToQuery GetBasePathMappings where
  toQuery GetBasePathMappings' {..} =
    Prelude.mconcat
      [ "position" Prelude.=: position,
        "limit" Prelude.=: limit
      ]

-- | Represents a collection of BasePathMapping resources.
--
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'newGetBasePathMappingsResponse' smart constructor.
data GetBasePathMappingsResponse = GetBasePathMappingsResponse'
  { -- | The current page of elements from this collection.
    items :: Prelude.Maybe [BasePathMapping],
    position :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBasePathMappingsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'getBasePathMappingsResponse_items' - The current page of elements from this collection.
--
-- 'position', 'getBasePathMappingsResponse_position' - Undocumented member.
--
-- 'httpStatus', 'getBasePathMappingsResponse_httpStatus' - The response's http status code.
newGetBasePathMappingsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBasePathMappingsResponse
newGetBasePathMappingsResponse pHttpStatus_ =
  GetBasePathMappingsResponse'
    { items =
        Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current page of elements from this collection.
getBasePathMappingsResponse_items :: Lens.Lens' GetBasePathMappingsResponse (Prelude.Maybe [BasePathMapping])
getBasePathMappingsResponse_items = Lens.lens (\GetBasePathMappingsResponse' {items} -> items) (\s@GetBasePathMappingsResponse' {} a -> s {items = a} :: GetBasePathMappingsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Undocumented member.
getBasePathMappingsResponse_position :: Lens.Lens' GetBasePathMappingsResponse (Prelude.Maybe Prelude.Text)
getBasePathMappingsResponse_position = Lens.lens (\GetBasePathMappingsResponse' {position} -> position) (\s@GetBasePathMappingsResponse' {} a -> s {position = a} :: GetBasePathMappingsResponse)

-- | The response's http status code.
getBasePathMappingsResponse_httpStatus :: Lens.Lens' GetBasePathMappingsResponse Prelude.Int
getBasePathMappingsResponse_httpStatus = Lens.lens (\GetBasePathMappingsResponse' {httpStatus} -> httpStatus) (\s@GetBasePathMappingsResponse' {} a -> s {httpStatus = a} :: GetBasePathMappingsResponse)

instance Prelude.NFData GetBasePathMappingsResponse
