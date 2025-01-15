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
-- Module      : Amazonka.CloudDirectory.CreateTypedLinkFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Amazonka.CloudDirectory.CreateTypedLinkFacet
  ( -- * Creating a Request
    CreateTypedLinkFacet (..),
    newCreateTypedLinkFacet,

    -- * Request Lenses
    createTypedLinkFacet_schemaArn,
    createTypedLinkFacet_facet,

    -- * Destructuring the Response
    CreateTypedLinkFacetResponse (..),
    newCreateTypedLinkFacetResponse,

    -- * Response Lenses
    createTypedLinkFacetResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTypedLinkFacet' smart constructor.
data CreateTypedLinkFacet = CreateTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | Facet structure that is associated with the typed link facet.
    facet :: TypedLinkFacet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTypedLinkFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'createTypedLinkFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'facet', 'createTypedLinkFacet_facet' - Facet structure that is associated with the typed link facet.
newCreateTypedLinkFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'facet'
  TypedLinkFacet ->
  CreateTypedLinkFacet
newCreateTypedLinkFacet pSchemaArn_ pFacet_ =
  CreateTypedLinkFacet'
    { schemaArn = pSchemaArn_,
      facet = pFacet_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
createTypedLinkFacet_schemaArn :: Lens.Lens' CreateTypedLinkFacet Prelude.Text
createTypedLinkFacet_schemaArn = Lens.lens (\CreateTypedLinkFacet' {schemaArn} -> schemaArn) (\s@CreateTypedLinkFacet' {} a -> s {schemaArn = a} :: CreateTypedLinkFacet)

-- | Facet structure that is associated with the typed link facet.
createTypedLinkFacet_facet :: Lens.Lens' CreateTypedLinkFacet TypedLinkFacet
createTypedLinkFacet_facet = Lens.lens (\CreateTypedLinkFacet' {facet} -> facet) (\s@CreateTypedLinkFacet' {} a -> s {facet = a} :: CreateTypedLinkFacet)

instance Core.AWSRequest CreateTypedLinkFacet where
  type
    AWSResponse CreateTypedLinkFacet =
      CreateTypedLinkFacetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTypedLinkFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTypedLinkFacet where
  hashWithSalt _salt CreateTypedLinkFacet' {..} =
    _salt
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` facet

instance Prelude.NFData CreateTypedLinkFacet where
  rnf CreateTypedLinkFacet' {..} =
    Prelude.rnf schemaArn `Prelude.seq`
      Prelude.rnf facet

instance Data.ToHeaders CreateTypedLinkFacet where
  toHeaders CreateTypedLinkFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON CreateTypedLinkFacet where
  toJSON CreateTypedLinkFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Facet" Data..= facet)]
      )

instance Data.ToPath CreateTypedLinkFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/create"

instance Data.ToQuery CreateTypedLinkFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTypedLinkFacetResponse' smart constructor.
data CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTypedLinkFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createTypedLinkFacetResponse_httpStatus' - The response's http status code.
newCreateTypedLinkFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTypedLinkFacetResponse
newCreateTypedLinkFacetResponse pHttpStatus_ =
  CreateTypedLinkFacetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createTypedLinkFacetResponse_httpStatus :: Lens.Lens' CreateTypedLinkFacetResponse Prelude.Int
createTypedLinkFacetResponse_httpStatus = Lens.lens (\CreateTypedLinkFacetResponse' {httpStatus} -> httpStatus) (\s@CreateTypedLinkFacetResponse' {} a -> s {httpStatus = a} :: CreateTypedLinkFacetResponse)

instance Prelude.NFData CreateTypedLinkFacetResponse where
  rnf CreateTypedLinkFacetResponse' {..} =
    Prelude.rnf httpStatus
