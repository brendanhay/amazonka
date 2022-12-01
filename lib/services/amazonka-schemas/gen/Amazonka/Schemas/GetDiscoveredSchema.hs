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
-- Module      : Amazonka.Schemas.GetDiscoveredSchema
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the discovered schema that was generated based on sampled events.
module Amazonka.Schemas.GetDiscoveredSchema
  ( -- * Creating a Request
    GetDiscoveredSchema (..),
    newGetDiscoveredSchema,

    -- * Request Lenses
    getDiscoveredSchema_type,
    getDiscoveredSchema_events,

    -- * Destructuring the Response
    GetDiscoveredSchemaResponse (..),
    newGetDiscoveredSchemaResponse,

    -- * Response Lenses
    getDiscoveredSchemaResponse_content,
    getDiscoveredSchemaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Schemas.Types

-- | /See:/ 'newGetDiscoveredSchema' smart constructor.
data GetDiscoveredSchema = GetDiscoveredSchema'
  { -- | The type of event.
    type' :: Type,
    -- | An array of strings where each string is a JSON event. These are the
    -- events that were used to generate the schema. The array includes a
    -- single type of event and has a maximum size of 10 events.
    events :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiscoveredSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'getDiscoveredSchema_type' - The type of event.
--
-- 'events', 'getDiscoveredSchema_events' - An array of strings where each string is a JSON event. These are the
-- events that were used to generate the schema. The array includes a
-- single type of event and has a maximum size of 10 events.
newGetDiscoveredSchema ::
  -- | 'type''
  Type ->
  -- | 'events'
  Prelude.NonEmpty Prelude.Text ->
  GetDiscoveredSchema
newGetDiscoveredSchema pType_ pEvents_ =
  GetDiscoveredSchema'
    { type' = pType_,
      events = Lens.coerced Lens.# pEvents_
    }

-- | The type of event.
getDiscoveredSchema_type :: Lens.Lens' GetDiscoveredSchema Type
getDiscoveredSchema_type = Lens.lens (\GetDiscoveredSchema' {type'} -> type') (\s@GetDiscoveredSchema' {} a -> s {type' = a} :: GetDiscoveredSchema)

-- | An array of strings where each string is a JSON event. These are the
-- events that were used to generate the schema. The array includes a
-- single type of event and has a maximum size of 10 events.
getDiscoveredSchema_events :: Lens.Lens' GetDiscoveredSchema (Prelude.NonEmpty Prelude.Text)
getDiscoveredSchema_events = Lens.lens (\GetDiscoveredSchema' {events} -> events) (\s@GetDiscoveredSchema' {} a -> s {events = a} :: GetDiscoveredSchema) Prelude.. Lens.coerced

instance Core.AWSRequest GetDiscoveredSchema where
  type
    AWSResponse GetDiscoveredSchema =
      GetDiscoveredSchemaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiscoveredSchemaResponse'
            Prelude.<$> (x Core..?> "Content")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDiscoveredSchema where
  hashWithSalt _salt GetDiscoveredSchema' {..} =
    _salt `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` events

instance Prelude.NFData GetDiscoveredSchema where
  rnf GetDiscoveredSchema' {..} =
    Prelude.rnf type' `Prelude.seq` Prelude.rnf events

instance Core.ToHeaders GetDiscoveredSchema where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDiscoveredSchema where
  toJSON GetDiscoveredSchema' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Type" Core..= type'),
            Prelude.Just ("Events" Core..= events)
          ]
      )

instance Core.ToPath GetDiscoveredSchema where
  toPath = Prelude.const "/v1/discover"

instance Core.ToQuery GetDiscoveredSchema where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDiscoveredSchemaResponse' smart constructor.
data GetDiscoveredSchemaResponse = GetDiscoveredSchemaResponse'
  { -- | The source of the schema definition.
    content :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDiscoveredSchemaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'content', 'getDiscoveredSchemaResponse_content' - The source of the schema definition.
--
-- 'httpStatus', 'getDiscoveredSchemaResponse_httpStatus' - The response's http status code.
newGetDiscoveredSchemaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDiscoveredSchemaResponse
newGetDiscoveredSchemaResponse pHttpStatus_ =
  GetDiscoveredSchemaResponse'
    { content =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The source of the schema definition.
getDiscoveredSchemaResponse_content :: Lens.Lens' GetDiscoveredSchemaResponse (Prelude.Maybe Prelude.Text)
getDiscoveredSchemaResponse_content = Lens.lens (\GetDiscoveredSchemaResponse' {content} -> content) (\s@GetDiscoveredSchemaResponse' {} a -> s {content = a} :: GetDiscoveredSchemaResponse)

-- | The response's http status code.
getDiscoveredSchemaResponse_httpStatus :: Lens.Lens' GetDiscoveredSchemaResponse Prelude.Int
getDiscoveredSchemaResponse_httpStatus = Lens.lens (\GetDiscoveredSchemaResponse' {httpStatus} -> httpStatus) (\s@GetDiscoveredSchemaResponse' {} a -> s {httpStatus = a} :: GetDiscoveredSchemaResponse)

instance Prelude.NFData GetDiscoveredSchemaResponse where
  rnf GetDiscoveredSchemaResponse' {..} =
    Prelude.rnf content
      `Prelude.seq` Prelude.rnf httpStatus
