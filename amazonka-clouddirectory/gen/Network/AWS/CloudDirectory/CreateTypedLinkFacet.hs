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
-- Module      : Network.AWS.CloudDirectory.CreateTypedLinkFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Network.AWS.CloudDirectory.CreateTypedLinkFacet
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateTypedLinkFacet' smart constructor.
data CreateTypedLinkFacet = CreateTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Core.Text,
    -- | Facet structure that is associated with the typed link facet.
    facet :: TypedLinkFacet
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
createTypedLinkFacet_schemaArn :: Lens.Lens' CreateTypedLinkFacet Core.Text
createTypedLinkFacet_schemaArn = Lens.lens (\CreateTypedLinkFacet' {schemaArn} -> schemaArn) (\s@CreateTypedLinkFacet' {} a -> s {schemaArn = a} :: CreateTypedLinkFacet)

-- | Facet structure that is associated with the typed link facet.
createTypedLinkFacet_facet :: Lens.Lens' CreateTypedLinkFacet TypedLinkFacet
createTypedLinkFacet_facet = Lens.lens (\CreateTypedLinkFacet' {facet} -> facet) (\s@CreateTypedLinkFacet' {} a -> s {facet = a} :: CreateTypedLinkFacet)

instance Core.AWSRequest CreateTypedLinkFacet where
  type
    AWSResponse CreateTypedLinkFacet =
      CreateTypedLinkFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateTypedLinkFacetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateTypedLinkFacet

instance Core.NFData CreateTypedLinkFacet

instance Core.ToHeaders CreateTypedLinkFacet where
  toHeaders CreateTypedLinkFacet' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# schemaArn]

instance Core.ToJSON CreateTypedLinkFacet where
  toJSON CreateTypedLinkFacet' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Facet" Core..= facet)])

instance Core.ToPath CreateTypedLinkFacet where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet/create"

instance Core.ToQuery CreateTypedLinkFacet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateTypedLinkFacetResponse' smart constructor.
data CreateTypedLinkFacetResponse = CreateTypedLinkFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateTypedLinkFacetResponse
newCreateTypedLinkFacetResponse pHttpStatus_ =
  CreateTypedLinkFacetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
createTypedLinkFacetResponse_httpStatus :: Lens.Lens' CreateTypedLinkFacetResponse Core.Int
createTypedLinkFacetResponse_httpStatus = Lens.lens (\CreateTypedLinkFacetResponse' {httpStatus} -> httpStatus) (\s@CreateTypedLinkFacetResponse' {} a -> s {httpStatus = a} :: CreateTypedLinkFacetResponse)

instance Core.NFData CreateTypedLinkFacetResponse
