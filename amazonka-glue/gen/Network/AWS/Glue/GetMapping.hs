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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetMapping' smart constructor.
data GetMapping = GetMapping'
  { -- | A list of target tables.
    sinks :: Core.Maybe [CatalogEntry],
    -- | Parameters for the mapping.
    location :: Core.Maybe Location,
    -- | Specifies the source table.
    source :: CatalogEntry
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { sinks = Core.Nothing,
      location = Core.Nothing,
      source = pSource_
    }

-- | A list of target tables.
getMapping_sinks :: Lens.Lens' GetMapping (Core.Maybe [CatalogEntry])
getMapping_sinks = Lens.lens (\GetMapping' {sinks} -> sinks) (\s@GetMapping' {} a -> s {sinks = a} :: GetMapping) Core.. Lens.mapping Lens._Coerce

-- | Parameters for the mapping.
getMapping_location :: Lens.Lens' GetMapping (Core.Maybe Location)
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
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Mapping" Core..!@ Core.mempty)
      )

instance Core.Hashable GetMapping

instance Core.NFData GetMapping

instance Core.ToHeaders GetMapping where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetMapping" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetMapping where
  toJSON GetMapping' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Sinks" Core..=) Core.<$> sinks,
            ("Location" Core..=) Core.<$> location,
            Core.Just ("Source" Core..= source)
          ]
      )

instance Core.ToPath GetMapping where
  toPath = Core.const "/"

instance Core.ToQuery GetMapping where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetMappingResponse' smart constructor.
data GetMappingResponse = GetMappingResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of mappings to the specified targets.
    mapping :: [MappingEntry]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetMappingResponse
newGetMappingResponse pHttpStatus_ =
  GetMappingResponse'
    { httpStatus = pHttpStatus_,
      mapping = Core.mempty
    }

-- | The response's http status code.
getMappingResponse_httpStatus :: Lens.Lens' GetMappingResponse Core.Int
getMappingResponse_httpStatus = Lens.lens (\GetMappingResponse' {httpStatus} -> httpStatus) (\s@GetMappingResponse' {} a -> s {httpStatus = a} :: GetMappingResponse)

-- | A list of mappings to the specified targets.
getMappingResponse_mapping :: Lens.Lens' GetMappingResponse [MappingEntry]
getMappingResponse_mapping = Lens.lens (\GetMappingResponse' {mapping} -> mapping) (\s@GetMappingResponse' {} a -> s {mapping = a} :: GetMappingResponse) Core.. Lens._Coerce

instance Core.NFData GetMappingResponse
