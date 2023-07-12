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
-- Module      : Amazonka.CloudDirectory.GetFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the Facet, such as facet name, attributes, Rules, or
-- @ObjectType@. You can call this on all kinds of schema facets --
-- published, development, or applied.
module Amazonka.CloudDirectory.GetFacet
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFacetResponse'
            Prelude.<$> (x Data..?> "Facet")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFacet where
  hashWithSalt _salt GetFacet' {..} =
    _salt
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetFacet where
  rnf GetFacet' {..} =
    Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders GetFacet where
  toHeaders GetFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON GetFacet where
  toJSON GetFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet"

instance Data.ToQuery GetFacet where
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

instance Prelude.NFData GetFacetResponse where
  rnf GetFacetResponse' {..} =
    Prelude.rnf facet
      `Prelude.seq` Prelude.rnf httpStatus
