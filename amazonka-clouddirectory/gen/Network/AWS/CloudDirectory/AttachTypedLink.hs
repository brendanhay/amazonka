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
-- Module      : Network.AWS.CloudDirectory.AttachTypedLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a typed link to a specified source and target object. For more
-- information, see
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links>.
module Network.AWS.CloudDirectory.AttachTypedLink
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

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachTypedLink' smart constructor.
data AttachTypedLink = AttachTypedLink'
  { -- | The Amazon Resource Name (ARN) of the directory where you want to attach
    -- the typed link.
    directoryArn :: Core.Text,
    -- | Identifies the source object that the typed link will attach to.
    sourceObjectReference :: ObjectReference,
    -- | Identifies the target object that the typed link will attach to.
    targetObjectReference :: ObjectReference,
    -- | Identifies the typed link facet that is associated with the typed link.
    typedLinkFacet :: TypedLinkSchemaAndFacetName,
    -- | A set of attributes that are associated with the typed link.
    attributes :: [AttributeNameAndValue]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
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
        attributes = Core.mempty
      }

-- | The Amazon Resource Name (ARN) of the directory where you want to attach
-- the typed link.
attachTypedLink_directoryArn :: Lens.Lens' AttachTypedLink Core.Text
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
attachTypedLink_attributes = Lens.lens (\AttachTypedLink' {attributes} -> attributes) (\s@AttachTypedLink' {} a -> s {attributes = a} :: AttachTypedLink) Core.. Lens._Coerce

instance Core.AWSRequest AttachTypedLink where
  type
    AWSResponse AttachTypedLink =
      AttachTypedLinkResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachTypedLinkResponse'
            Core.<$> (x Core..?> "TypedLinkSpecifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachTypedLink

instance Core.NFData AttachTypedLink

instance Core.ToHeaders AttachTypedLink where
  toHeaders AttachTypedLink' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON AttachTypedLink where
  toJSON AttachTypedLink' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "SourceObjectReference"
                  Core..= sourceObjectReference
              ),
            Core.Just
              ( "TargetObjectReference"
                  Core..= targetObjectReference
              ),
            Core.Just ("TypedLinkFacet" Core..= typedLinkFacet),
            Core.Just ("Attributes" Core..= attributes)
          ]
      )

instance Core.ToPath AttachTypedLink where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/typedlink/attach"

instance Core.ToQuery AttachTypedLink where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachTypedLinkResponse' smart constructor.
data AttachTypedLinkResponse = AttachTypedLinkResponse'
  { -- | Returns a typed link specifier as output.
    typedLinkSpecifier :: Core.Maybe TypedLinkSpecifier,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  AttachTypedLinkResponse
newAttachTypedLinkResponse pHttpStatus_ =
  AttachTypedLinkResponse'
    { typedLinkSpecifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a typed link specifier as output.
attachTypedLinkResponse_typedLinkSpecifier :: Lens.Lens' AttachTypedLinkResponse (Core.Maybe TypedLinkSpecifier)
attachTypedLinkResponse_typedLinkSpecifier = Lens.lens (\AttachTypedLinkResponse' {typedLinkSpecifier} -> typedLinkSpecifier) (\s@AttachTypedLinkResponse' {} a -> s {typedLinkSpecifier = a} :: AttachTypedLinkResponse)

-- | The response's http status code.
attachTypedLinkResponse_httpStatus :: Lens.Lens' AttachTypedLinkResponse Core.Int
attachTypedLinkResponse_httpStatus = Lens.lens (\AttachTypedLinkResponse' {httpStatus} -> httpStatus) (\s@AttachTypedLinkResponse' {} a -> s {httpStatus = a} :: AttachTypedLinkResponse)

instance Core.NFData AttachTypedLinkResponse
