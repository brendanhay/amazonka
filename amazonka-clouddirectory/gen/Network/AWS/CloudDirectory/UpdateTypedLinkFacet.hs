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
-- Module      : Network.AWS.CloudDirectory.UpdateTypedLinkFacet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a TypedLinkFacet. For more information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Network.AWS.CloudDirectory.UpdateTypedLinkFacet
  ( -- * Creating a Request
    UpdateTypedLinkFacet (..),
    newUpdateTypedLinkFacet,

    -- * Request Lenses
    updateTypedLinkFacet_schemaArn,
    updateTypedLinkFacet_name,
    updateTypedLinkFacet_attributeUpdates,
    updateTypedLinkFacet_identityAttributeOrder,

    -- * Destructuring the Response
    UpdateTypedLinkFacetResponse (..),
    newUpdateTypedLinkFacetResponse,

    -- * Response Lenses
    updateTypedLinkFacetResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateTypedLinkFacet' smart constructor.
data UpdateTypedLinkFacet = UpdateTypedLinkFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For
    -- more information, see arns.
    schemaArn :: Prelude.Text,
    -- | The unique name of the typed link facet.
    name :: Prelude.Text,
    -- | Attributes update structure.
    attributeUpdates :: [TypedLinkFacetAttributeUpdate],
    -- | The order of identity attributes for the facet, from most significant to
    -- least significant. The ability to filter typed links considers the order
    -- that the attributes are defined on the typed link facet. When providing
    -- ranges to a typed link selection, any inexact ranges must be specified
    -- at the end. Any attributes that do not have a range specified are
    -- presumed to match the entire range. Filters are interpreted in the order
    -- of the attributes on the typed link facet, not the order in which they
    -- are supplied to any API calls. For more information about identity
    -- attributes, see
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
    identityAttributeOrder :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTypedLinkFacet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaArn', 'updateTypedLinkFacet_schemaArn' - The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
--
-- 'name', 'updateTypedLinkFacet_name' - The unique name of the typed link facet.
--
-- 'attributeUpdates', 'updateTypedLinkFacet_attributeUpdates' - Attributes update structure.
--
-- 'identityAttributeOrder', 'updateTypedLinkFacet_identityAttributeOrder' - The order of identity attributes for the facet, from most significant to
-- least significant. The ability to filter typed links considers the order
-- that the attributes are defined on the typed link facet. When providing
-- ranges to a typed link selection, any inexact ranges must be specified
-- at the end. Any attributes that do not have a range specified are
-- presumed to match the entire range. Filters are interpreted in the order
-- of the attributes on the typed link facet, not the order in which they
-- are supplied to any API calls. For more information about identity
-- attributes, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
newUpdateTypedLinkFacet ::
  -- | 'schemaArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateTypedLinkFacet
newUpdateTypedLinkFacet pSchemaArn_ pName_ =
  UpdateTypedLinkFacet'
    { schemaArn = pSchemaArn_,
      name = pName_,
      attributeUpdates = Prelude.mempty,
      identityAttributeOrder = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For
-- more information, see arns.
updateTypedLinkFacet_schemaArn :: Lens.Lens' UpdateTypedLinkFacet Prelude.Text
updateTypedLinkFacet_schemaArn = Lens.lens (\UpdateTypedLinkFacet' {schemaArn} -> schemaArn) (\s@UpdateTypedLinkFacet' {} a -> s {schemaArn = a} :: UpdateTypedLinkFacet)

-- | The unique name of the typed link facet.
updateTypedLinkFacet_name :: Lens.Lens' UpdateTypedLinkFacet Prelude.Text
updateTypedLinkFacet_name = Lens.lens (\UpdateTypedLinkFacet' {name} -> name) (\s@UpdateTypedLinkFacet' {} a -> s {name = a} :: UpdateTypedLinkFacet)

-- | Attributes update structure.
updateTypedLinkFacet_attributeUpdates :: Lens.Lens' UpdateTypedLinkFacet [TypedLinkFacetAttributeUpdate]
updateTypedLinkFacet_attributeUpdates = Lens.lens (\UpdateTypedLinkFacet' {attributeUpdates} -> attributeUpdates) (\s@UpdateTypedLinkFacet' {} a -> s {attributeUpdates = a} :: UpdateTypedLinkFacet) Prelude.. Prelude._Coerce

-- | The order of identity attributes for the facet, from most significant to
-- least significant. The ability to filter typed links considers the order
-- that the attributes are defined on the typed link facet. When providing
-- ranges to a typed link selection, any inexact ranges must be specified
-- at the end. Any attributes that do not have a range specified are
-- presumed to match the entire range. Filters are interpreted in the order
-- of the attributes on the typed link facet, not the order in which they
-- are supplied to any API calls. For more information about identity
-- attributes, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
updateTypedLinkFacet_identityAttributeOrder :: Lens.Lens' UpdateTypedLinkFacet [Prelude.Text]
updateTypedLinkFacet_identityAttributeOrder = Lens.lens (\UpdateTypedLinkFacet' {identityAttributeOrder} -> identityAttributeOrder) (\s@UpdateTypedLinkFacet' {} a -> s {identityAttributeOrder = a} :: UpdateTypedLinkFacet) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest UpdateTypedLinkFacet where
  type
    Rs UpdateTypedLinkFacet =
      UpdateTypedLinkFacetResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTypedLinkFacetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTypedLinkFacet

instance Prelude.NFData UpdateTypedLinkFacet

instance Prelude.ToHeaders UpdateTypedLinkFacet where
  toHeaders UpdateTypedLinkFacet' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# schemaArn]

instance Prelude.ToJSON UpdateTypedLinkFacet where
  toJSON UpdateTypedLinkFacet' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Prelude..= name),
            Prelude.Just
              ("AttributeUpdates" Prelude..= attributeUpdates),
            Prelude.Just
              ( "IdentityAttributeOrder"
                  Prelude..= identityAttributeOrder
              )
          ]
      )

instance Prelude.ToPath UpdateTypedLinkFacet where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/facet"

instance Prelude.ToQuery UpdateTypedLinkFacet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTypedLinkFacetResponse' smart constructor.
data UpdateTypedLinkFacetResponse = UpdateTypedLinkFacetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateTypedLinkFacetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTypedLinkFacetResponse_httpStatus' - The response's http status code.
newUpdateTypedLinkFacetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTypedLinkFacetResponse
newUpdateTypedLinkFacetResponse pHttpStatus_ =
  UpdateTypedLinkFacetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateTypedLinkFacetResponse_httpStatus :: Lens.Lens' UpdateTypedLinkFacetResponse Prelude.Int
updateTypedLinkFacetResponse_httpStatus = Lens.lens (\UpdateTypedLinkFacetResponse' {httpStatus} -> httpStatus) (\s@UpdateTypedLinkFacetResponse' {} a -> s {httpStatus = a} :: UpdateTypedLinkFacetResponse)

instance Prelude.NFData UpdateTypedLinkFacetResponse
