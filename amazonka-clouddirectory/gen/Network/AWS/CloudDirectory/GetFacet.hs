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
-- Module      : Network.AWS.CloudDirectory.GetFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the Facet, such as facet name, attributes, Rules, or
-- @ObjectType@. You can call this on all kinds of schema facets --
-- published, development, or applied.
module Network.AWS.CloudDirectory.GetFacet
  ( -- * Creating a Request
    GetFacet (..),
    newGetFacet,

    -- * Request Lenses
    getFacet_schemaArn,
    getFacet_name,

    -- * Destructuring the Response
    GetFacetResponse (..),
    newGetFacetResponse,

    -- * Response Lenses
    getFacetResponse_facet,
    getFacetResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFacet' smart constructor.
data GetFacet = GetFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the Facet. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The name of the facet to retrieve.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'getFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
--
-- 'name', 'getFacet_name' - The name of the facet to retrieve.
newGetFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  GetFacet
newGetFacet pSchemaArn_ pName_ =
  GetFacet' {schemaArn = pSchemaArn_, name = pName_}

-- | The Amazon Resource Name (ARN) that is associated with the Facet. For
-- more information, see arns.
getFacet_schemaArn :: Lens.Lens' GetFacet Prelude.Text
getFacet_schemaArn = Lens.lens (\GetFacet' {schemaArn} -> schemaArn) (\s@GetFacet' {} a -> s {schemaArn = a} :: GetFacet)

-- | The name of the facet to retrieve.
getFacet_name :: Lens.Lens' GetFacet Prelude.Text
getFacet_name = Lens.lens (\GetFacet' {name} -> name) (\s@GetFacet' {} a -> s {name = a} :: GetFacet)

instance Core.AWSRequest GetFacet where
  type AWSResponse GetFacet = GetFacetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFacetResponse'
            Prelude.<$> (x Core..?> "Facet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFacet

instance Prelude.NFData GetFacet

instance Core.ToHeaders GetFacet where
  toHeaders GetFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON GetFacet where
  toJSON GetFacet' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath GetFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet"

instance Core.ToQuery GetFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFacetResponse' smart constructor.
data GetFacetResponse = GetFacetResponse'
  { -- | The Facet structure that is associated with the facet.
    facet :: Prelude.Maybe Facet,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'facet', 'getFacetResponse_facet' - The Facet structure that is associated with the facet.
--
-- 'httpStatus', 'getFacetResponse_httpStatus' - The response's http status code.
newGetFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFacetResponse
newGetFacetResponse pHttpStatus_ =
  GetFacetResponse'
    { facet = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Facet structure that is associated with the facet.
getFacetResponse_facet :: Lens.Lens' GetFacetResponse (Prelude.Maybe Facet)
getFacetResponse_facet = Lens.lens (\GetFacetResponse' {facet} -> facet) (\s@GetFacetResponse' {} a -> s {facet = a} :: GetFacetResponse)

-- | The response's http status code.
getFacetResponse_httpStatus :: Lens.Lens' GetFacetResponse Prelude.Int
getFacetResponse_httpStatus = Lens.lens (\GetFacetResponse' {httpStatus} -> httpStatus) (\s@GetFacetResponse' {} a -> s {httpStatus = a} :: GetFacetResponse)

instance Prelude.NFData GetFacetResponse
