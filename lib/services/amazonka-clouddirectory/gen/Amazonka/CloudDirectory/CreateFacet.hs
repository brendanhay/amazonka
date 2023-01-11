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
-- Module      : Amazonka.CloudDirectory.CreateFacet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Facet in a schema. Facet creation is allowed only in
-- development or applied schemas.
module Amazonka.CloudDirectory.CreateFacet
  ( -- * Creating a Request
    CreateFacet (..),
    newCreateFacet,

    -- * Request Lenses
    createFacet_attributes,
    createFacet_facetStyle,
    createFacet_objectType,
    createFacet_schemaArn,
    createFacet_name,

    -- * Destructuring the Response
    CreateFacetResponse (..),
    newCreateFacetResponse,

    -- * Response Lenses
    createFacetResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateFacet' smart constructor.
data CreateFacet = CreateFacet'
  { -- | The attributes that are associated with the Facet.
    attributes :: Prelude.Maybe [FacetAttribute],
    -- | There are two different styles that you can define on any given facet,
    -- @Static@ and @Dynamic@. For static facets, all attributes must be
    -- defined in the schema. For dynamic facets, attributes can be defined
    -- during data plane operations.
    facetStyle :: Prelude.Maybe FacetStyle,
    -- | Specifies whether a given object created from this facet is of type
    -- node, leaf node, policy or index.
    --
    -- -   Node: Can have multiple children but one parent.
    --
    -- -   Leaf node: Cannot have children but can have multiple parents.
    --
    -- -   Policy: Allows you to store a policy document and policy type. For
    --     more information, see
    --     <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
    --
    -- -   Index: Can be created with the Index API.
    objectType :: Prelude.Maybe ObjectType,
    -- | The schema ARN in which the new Facet will be created. For more
    -- information, see arns.
    schemaArn :: Prelude.Text,
    -- | The name of the Facet, which is unique for a given schema.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'createFacet_attributes' - The attributes that are associated with the Facet.
--
-- 'facetStyle', 'createFacet_facetStyle' - There are two different styles that you can define on any given facet,
-- @Static@ and @Dynamic@. For static facets, all attributes must be
-- defined in the schema. For dynamic facets, attributes can be defined
-- during data plane operations.
--
-- 'objectType', 'createFacet_objectType' - Specifies whether a given object created from this facet is of type
-- node, leaf node, policy or index.
--
-- -   Node: Can have multiple children but one parent.
--
-- -   Leaf node: Cannot have children but can have multiple parents.
--
-- -   Policy: Allows you to store a policy document and policy type. For
--     more information, see
--     <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- -   Index: Can be created with the Index API.
--
-- 'schemaArn', 'createFacet_schemaArn' - The schema ARN in which the new Facet will be created. For more
-- information, see arns.
--
-- 'name', 'createFacet_name' - The name of the Facet, which is unique for a given schema.
newCreateFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateFacet
newCreateFacet pSchemaArn_ pName_ =
  CreateFacet'
    { attributes = Prelude.Nothing,
      facetStyle = Prelude.Nothing,
      objectType = Prelude.Nothing,
      schemaArn = pSchemaArn_,
      name = pName_
    }

-- | The attributes that are associated with the Facet.
createFacet_attributes :: Lens.Lens' CreateFacet (Prelude.Maybe [FacetAttribute])
createFacet_attributes = Lens.lens (\CreateFacet' {attributes} -> attributes) (\s@CreateFacet' {} a -> s {attributes = a} :: CreateFacet) Prelude.. Lens.mapping Lens.coerced

-- | There are two different styles that you can define on any given facet,
-- @Static@ and @Dynamic@. For static facets, all attributes must be
-- defined in the schema. For dynamic facets, attributes can be defined
-- during data plane operations.
createFacet_facetStyle :: Lens.Lens' CreateFacet (Prelude.Maybe FacetStyle)
createFacet_facetStyle = Lens.lens (\CreateFacet' {facetStyle} -> facetStyle) (\s@CreateFacet' {} a -> s {facetStyle = a} :: CreateFacet)

-- | Specifies whether a given object created from this facet is of type
-- node, leaf node, policy or index.
--
-- -   Node: Can have multiple children but one parent.
--
-- -   Leaf node: Cannot have children but can have multiple parents.
--
-- -   Policy: Allows you to store a policy document and policy type. For
--     more information, see
--     <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/key_concepts_directory.html#key_concepts_policies Policies>.
--
-- -   Index: Can be created with the Index API.
createFacet_objectType :: Lens.Lens' CreateFacet (Prelude.Maybe ObjectType)
createFacet_objectType = Lens.lens (\CreateFacet' {objectType} -> objectType) (\s@CreateFacet' {} a -> s {objectType = a} :: CreateFacet)

-- | The schema ARN in which the new Facet will be created. For more
-- information, see arns.
createFacet_schemaArn :: Lens.Lens' CreateFacet Prelude.Text
createFacet_schemaArn = Lens.lens (\CreateFacet' {schemaArn} -> schemaArn) (\s@CreateFacet' {} a -> s {schemaArn = a} :: CreateFacet)

-- | The name of the Facet, which is unique for a given schema.
createFacet_name :: Lens.Lens' CreateFacet Prelude.Text
createFacet_name = Lens.lens (\CreateFacet' {name} -> name) (\s@CreateFacet' {} a -> s {name = a} :: CreateFacet)

instance Core.AWSRequest CreateFacet where
  type AWSResponse CreateFacet = CreateFacetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateFacet where
  hashWithSalt _salt CreateFacet' {..} =
    _salt `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` facetStyle
      `Prelude.hashWithSalt` objectType
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateFacet where
  rnf CreateFacet' {..} =
    Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf facetStyle
      `Prelude.seq` Prelude.rnf objectType
      `Prelude.seq` Prelude.rnf schemaArn
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateFacet where
  toHeaders CreateFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# schemaArn]

instance Data.ToJSON CreateFacet where
  toJSON CreateFacet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Attributes" Data..=) Prelude.<$> attributes,
            ("FacetStyle" Data..=) Prelude.<$> facetStyle,
            ("ObjectType" Data..=) Prelude.<$> objectType,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath CreateFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/facet/create"

instance Data.ToQuery CreateFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateFacetResponse' smart constructor.
data CreateFacetResponse = CreateFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createFacetResponse_httpStatus' - The response's http status code.
newCreateFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateFacetResponse
newCreateFacetResponse pHttpStatus_ =
  CreateFacetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
createFacetResponse_httpStatus :: Lens.Lens' CreateFacetResponse Prelude.Int
createFacetResponse_httpStatus = Lens.lens (\CreateFacetResponse' {httpStatus} -> httpStatus) (\s@CreateFacetResponse' {} a -> s {httpStatus = a} :: CreateFacetResponse)

instance Prelude.NFData CreateFacetResponse where
  rnf CreateFacetResponse' {..} = Prelude.rnf httpStatus
