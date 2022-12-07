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
-- Module      : Amazonka.Glue.GetMapping
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates mappings.
module Amazonka.Glue.GetMapping
  ( -- * Creating a Request
    GetMapping (..),
    newGetMapping,

    -- * Request Lenses
    getMapping_location,
    getMapping_sinks,
    getMapping_source,

    -- * Destructuring the Response
    GetMappingResponse (..),
    newGetMappingResponse,

    -- * Response Lenses
    getMappingResponse_httpStatus,
    getMappingResponse_mapping,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetMapping' smart constructor.
data GetMapping = GetMapping'
  { -- | Parameters for the mapping.
    location :: Prelude.Maybe Location,
    -- | A list of target tables.
    sinks :: Prelude.Maybe [CatalogEntry],
    -- | Specifies the source table.
    source :: CatalogEntry
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'getMapping_location' - Parameters for the mapping.
--
-- 'sinks', 'getMapping_sinks' - A list of target tables.
--
-- 'source', 'getMapping_source' - Specifies the source table.
newGetMapping ::
  -- | 'source'
  CatalogEntry ->
  GetMapping
newGetMapping pSource_ =
  GetMapping'
    { location = Prelude.Nothing,
      sinks = Prelude.Nothing,
      source = pSource_
    }

-- | Parameters for the mapping.
getMapping_location :: Lens.Lens' GetMapping (Prelude.Maybe Location)
getMapping_location = Lens.lens (\GetMapping' {location} -> location) (\s@GetMapping' {} a -> s {location = a} :: GetMapping)

-- | A list of target tables.
getMapping_sinks :: Lens.Lens' GetMapping (Prelude.Maybe [CatalogEntry])
getMapping_sinks = Lens.lens (\GetMapping' {sinks} -> sinks) (\s@GetMapping' {} a -> s {sinks = a} :: GetMapping) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the source table.
getMapping_source :: Lens.Lens' GetMapping CatalogEntry
getMapping_source = Lens.lens (\GetMapping' {source} -> source) (\s@GetMapping' {} a -> s {source = a} :: GetMapping)

instance Core.AWSRequest GetMapping where
  type AWSResponse GetMapping = GetMappingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMappingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "Mapping" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetMapping where
  hashWithSalt _salt GetMapping' {..} =
    _salt `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` sinks
      `Prelude.hashWithSalt` source

instance Prelude.NFData GetMapping where
  rnf GetMapping' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf sinks
      `Prelude.seq` Prelude.rnf source

instance Data.ToHeaders GetMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetMapping" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetMapping where
  toJSON GetMapping' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Location" Data..=) Prelude.<$> location,
            ("Sinks" Data..=) Prelude.<$> sinks,
            Prelude.Just ("Source" Data..= source)
          ]
      )

instance Data.ToPath GetMapping where
  toPath = Prelude.const "/"

instance Data.ToQuery GetMapping where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetMappingResponse' smart constructor.
data GetMappingResponse = GetMappingResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of mappings to the specified targets.
    mapping :: [MappingEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetMappingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getMappingResponse_httpStatus' - The response's http status code.
--
-- 'mapping', 'getMappingResponse_mapping' - A list of mappings to the specified targets.
newGetMappingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetMappingResponse
newGetMappingResponse pHttpStatus_ =
  GetMappingResponse'
    { httpStatus = pHttpStatus_,
      mapping = Prelude.mempty
    }

-- | The response's http status code.
getMappingResponse_httpStatus :: Lens.Lens' GetMappingResponse Prelude.Int
getMappingResponse_httpStatus = Lens.lens (\GetMappingResponse' {httpStatus} -> httpStatus) (\s@GetMappingResponse' {} a -> s {httpStatus = a} :: GetMappingResponse)

-- | A list of mappings to the specified targets.
getMappingResponse_mapping :: Lens.Lens' GetMappingResponse [MappingEntry]
getMappingResponse_mapping = Lens.lens (\GetMappingResponse' {mapping} -> mapping) (\s@GetMappingResponse' {} a -> s {mapping = a} :: GetMappingResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetMappingResponse where
  rnf GetMappingResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mapping
