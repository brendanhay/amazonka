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
-- Module      : Network.AWS.Glue.GetMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates mappings.
module Network.AWS.Glue.GetMapping
  ( -- * Creating a Request
    GetMapping (..),
    newGetMapping,

    -- * Request Lenses
    getMapping_sinks,
    getMapping_location,
    getMapping_source,

    -- * Destructuring the Response
    GetMappingResponse (..),
    newGetMappingResponse,

    -- * Response Lenses
    getMappingResponse_httpStatus,
    getMappingResponse_mapping,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMapping' smart constructor.
data GetMapping = GetMapping'
  { -- | A list of target tables.
    sinks :: Prelude.Maybe [CatalogEntry],
    -- | Parameters for the mapping.
    location :: Prelude.Maybe Location,
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
-- 'sinks', 'getMapping_sinks' - A list of target tables.
--
-- 'location', 'getMapping_location' - Parameters for the mapping.
--
-- 'source', 'getMapping_source' - Specifies the source table.
newGetMapping ::
  -- | 'source'
  CatalogEntry ->
  GetMapping
newGetMapping pSource_ =
  GetMapping'
    { sinks = Prelude.Nothing,
      location = Prelude.Nothing,
      source = pSource_
    }

-- | A list of target tables.
getMapping_sinks :: Lens.Lens' GetMapping (Prelude.Maybe [CatalogEntry])
getMapping_sinks = Lens.lens (\GetMapping' {sinks} -> sinks) (\s@GetMapping' {} a -> s {sinks = a} :: GetMapping) Prelude.. Lens.mapping Lens._Coerce

-- | Parameters for the mapping.
getMapping_location :: Lens.Lens' GetMapping (Prelude.Maybe Location)
getMapping_location = Lens.lens (\GetMapping' {location} -> location) (\s@GetMapping' {} a -> s {location = a} :: GetMapping)

-- | Specifies the source table.
getMapping_source :: Lens.Lens' GetMapping CatalogEntry
getMapping_source = Lens.lens (\GetMapping' {source} -> source) (\s@GetMapping' {} a -> s {source = a} :: GetMapping)

instance Core.AWSRequest GetMapping where
  type AWSResponse GetMapping = GetMappingResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetMappingResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Mapping" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetMapping

instance Prelude.NFData GetMapping

instance Core.ToHeaders GetMapping where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMapping" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetMapping where
  toJSON GetMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Sinks" Core..=) Prelude.<$> sinks,
            ("Location" Core..=) Prelude.<$> location,
            Prelude.Just ("Source" Core..= source)
          ]
      )

instance Core.ToPath GetMapping where
  toPath = Prelude.const "/"

instance Core.ToQuery GetMapping where
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
getMappingResponse_mapping = Lens.lens (\GetMappingResponse' {mapping} -> mapping) (\s@GetMappingResponse' {} a -> s {mapping = a} :: GetMappingResponse) Prelude.. Lens._Coerce

instance Prelude.NFData GetMappingResponse
