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
-- Module      : Amazonka.CloudDirectory.AttachTypedLink
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Amazonka.CloudDirectory.AttachTypedLink
  ( -- * Creating a Request
    AttachTypedLink (..),
    newAttachTypedLink,

    -- * Request Lenses
    attachTypedLink_directoryArn,
    attachTypedLink_sourceObjectReference,
    attachTypedLink_targetObjectReference,
    attachTypedLink_typedLinkFacet,
    attachTypedLink_attributes,

    -- * Destructuring the Response
    AttachTypedLinkResponse (..),
    newAttachTypedLinkResponse,

    -- * Response Lenses
    attachTypedLinkResponse_typedLinkSpecifier,
    attachTypedLinkResponse_httpStatus,
  )
where

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachTypedLink' smart constructor.
data AttachTypedLink = AttachTypedLink'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to attach
    -- the typed link.
    directoryArn :: Prelude.Text,
    -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: ObjectReference,
    -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: TypedLinkSchemaAndFacetName,
    -- | A set of attributes that are associated with the typed link.
    attributes :: [AttributeNameAndValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachTypedLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'attachTypedLink_directoryArn' - The Amazon Resource Name (ARN) of the directory where you want to attach
-- the typed link.
--
-- 'sourceObjectReference', 'attachTypedLink_sourceObjectReference' - Identifies the source object that the typed link will attach to.
--
-- 'targetObjectReference', 'attachTypedLink_targetObjectReference' - Identifies the target object that the typed link will attach to.
--
-- 'typedLinkFacet', 'attachTypedLink_typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
--
-- 'attributes', 'attachTypedLink_attributes' - A set of attributes that are associated with the typed link.
newAttachTypedLink ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'targetObjectReference'
  ObjectReference ->
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  AttachTypedLink
newAttachTypedLink
  pDirectoryArn_
  pSourceObjectReference_
  pTargetObjectReference_
  pTypedLinkFacet_ =
    AttachTypedLink'
      { directoryArn = pDirectoryArn_,
        sourceObjectReference = pSourceObjectReference_,
        targetObjectReference = pTargetObjectReference_,
        typedLinkFacet = pTypedLinkFacet_,
        attributes = Prelude.mempty
      }

-- | The Amazon Resource Name (ARN) of the directory where you want to attach
-- the typed link.
attachTypedLink_directoryArn :: Lens.Lens' AttachTypedLink Prelude.Text
attachTypedLink_directoryArn = Lens.lens (\AttachTypedLink' {directoryArn} -> directoryArn) (\s@AttachTypedLink' {} a -> s {directoryArn = a} :: AttachTypedLink)

-- | Identifies the source object that the typed link will attach to.
attachTypedLink_sourceObjectReference :: Lens.Lens' AttachTypedLink ObjectReference
attachTypedLink_sourceObjectReference = Lens.lens (\AttachTypedLink' {sourceObjectReference} -> sourceObjectReference) (\s@AttachTypedLink' {} a -> s {sourceObjectReference = a} :: AttachTypedLink)

-- | Identifies the target object that the typed link will attach to.
attachTypedLink_targetObjectReference :: Lens.Lens' AttachTypedLink ObjectReference
attachTypedLink_targetObjectReference = Lens.lens (\AttachTypedLink' {targetObjectReference} -> targetObjectReference) (\s@AttachTypedLink' {} a -> s {targetObjectReference = a} :: AttachTypedLink)

-- | Identifies the typed link facet that is associated with the typed link.
attachTypedLink_typedLinkFacet :: Lens.Lens' AttachTypedLink TypedLinkSchemaAndFacetName
attachTypedLink_typedLinkFacet = Lens.lens (\AttachTypedLink' {typedLinkFacet} -> typedLinkFacet) (\s@AttachTypedLink' {} a -> s {typedLinkFacet = a} :: AttachTypedLink)

-- | A set of attributes that are associated with the typed link.
attachTypedLink_attributes :: Lens.Lens' AttachTypedLink [AttributeNameAndValue]
attachTypedLink_attributes = Lens.lens (\AttachTypedLink' {attributes} -> attributes) (\s@AttachTypedLink' {} a -> s {attributes = a} :: AttachTypedLink) Prelude.. Lens.coerced

instance Core.AWSRequest AttachTypedLink where
  type
    AWSResponse AttachTypedLink =
      AttachTypedLinkResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachTypedLinkResponse'
            Prelude.<$> (x Core..?> "TypedLinkSpecifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachTypedLink where
  hashWithSalt _salt AttachTypedLink' {..} =
    _salt `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` sourceObjectReference
      `Prelude.hashWithSalt` targetObjectReference
      `Prelude.hashWithSalt` typedLinkFacet
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData AttachTypedLink where
  rnf AttachTypedLink' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf sourceObjectReference
      `Prelude.seq` Prelude.rnf targetObjectReference
      `Prelude.seq` Prelude.rnf typedLinkFacet
      `Prelude.seq` Prelude.rnf attributes

instance Core.ToHeaders AttachTypedLink where
  toHeaders AttachTypedLink' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON AttachTypedLink where
  toJSON AttachTypedLink' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "SourceObjectReference"
                  Core..= sourceObjectReference
              ),
            Prelude.Just
              ( "TargetObjectReference"
                  Core..= targetObjectReference
              ),
            Prelude.Just
              ("TypedLinkFacet" Core..= typedLinkFacet),
            Prelude.Just ("Attributes" Core..= attributes)
          ]
      )

instance Core.ToPath AttachTypedLink where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/typedlink/attach"

instance Core.ToQuery AttachTypedLink where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachTypedLinkResponse' smart constructor.
data AttachTypedLinkResponse = AttachTypedLinkResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifier :: Prelude.Maybe TypedLinkSpecifier,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachTypedLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'typedLinkSpecifier', 'attachTypedLinkResponse_typedLinkSpecifier' - Returns a typed link specifier as output.
--
-- 'httpStatus', 'attachTypedLinkResponse_httpStatus' - The response's http status code.
newAttachTypedLinkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachTypedLinkResponse
newAttachTypedLinkResponse pHttpStatus_ =
  AttachTypedLinkResponse'
    { typedLinkSpecifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a typed link specifier as output.
attachTypedLinkResponse_typedLinkSpecifier :: Lens.Lens' AttachTypedLinkResponse (Prelude.Maybe TypedLinkSpecifier)
attachTypedLinkResponse_typedLinkSpecifier = Lens.lens (\AttachTypedLinkResponse' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@AttachTypedLinkResponse' {} a -> s {typedLinkSpecifier = a} :: AttachTypedLinkResponse)

-- | The response's http status code.
attachTypedLinkResponse_httpStatus :: Lens.Lens' AttachTypedLinkResponse Prelude.Int
attachTypedLinkResponse_httpStatus = Lens.lens (\AttachTypedLinkResponse' {httpStatus} -> httpStatus) (\s@AttachTypedLinkResponse' {} a -> s {httpStatus = a} :: AttachTypedLinkResponse)

instance Prelude.NFData AttachTypedLinkResponse where
  rnf AttachTypedLinkResponse' {..} =
    Prelude.rnf typedLinkSpecifier
      `Prelude.seq` Prelude.rnf httpStatus
