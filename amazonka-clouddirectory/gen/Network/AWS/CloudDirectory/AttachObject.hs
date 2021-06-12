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
-- Module      : Network.AWS.CloudDirectory.AttachObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an existing object to another object. An object can be accessed
-- in two ways:
--
-- 1.  Using the path
--
-- 2.  Using @ObjectIdentifier@
module Network.AWS.CloudDirectory.AttachObject
  ( -- * Creating a Request
    AttachObject (..),
    newAttachObject,

    -- * Request Lenses
    attachObject_directoryArn,
    attachObject_parentReference,
    attachObject_childReference,
    attachObject_linkName,

    -- * Destructuring the Response
    AttachObjectResponse (..),
    newAttachObjectResponse,

    -- * Response Lenses
    attachObjectResponse_attachedObjectIdentifier,
    attachObjectResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachObject' smart constructor.
data AttachObject = AttachObject'
  { -- | Amazon Resource Name (ARN) that is associated with the Directory where
    -- both objects reside. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The parent object reference.
    parentReference :: ObjectReference,
    -- | The child object reference to be attached to the object.
    childReference :: ObjectReference,
    -- | The link name with which the child object is attached to the parent.
    linkName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'attachObject_directoryArn' - Amazon Resource Name (ARN) that is associated with the Directory where
-- both objects reside. For more information, see arns.
--
-- 'parentReference', 'attachObject_parentReference' - The parent object reference.
--
-- 'childReference', 'attachObject_childReference' - The child object reference to be attached to the object.
--
-- 'linkName', 'attachObject_linkName' - The link name with which the child object is attached to the parent.
newAttachObject ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'childReference'
  ObjectReference ->
  -- | 'linkName'
  Core.Text ->
  AttachObject
newAttachObject
  pDirectoryArn_
  pParentReference_
  pChildReference_
  pLinkName_ =
    AttachObject'
      { directoryArn = pDirectoryArn_,
        parentReference = pParentReference_,
        childReference = pChildReference_,
        linkName = pLinkName_
      }

-- | Amazon Resource Name (ARN) that is associated with the Directory where
-- both objects reside. For more information, see arns.
attachObject_directoryArn :: Lens.Lens' AttachObject Core.Text
attachObject_directoryArn = Lens.lens (\AttachObject' {directoryArn} -> directoryArn) (\s@AttachObject' {} a -> s {directoryArn = a} :: AttachObject)

-- | The parent object reference.
attachObject_parentReference :: Lens.Lens' AttachObject ObjectReference
attachObject_parentReference = Lens.lens (\AttachObject' {parentReference} -> parentReference) (\s@AttachObject' {} a -> s {parentReference = a} :: AttachObject)

-- | The child object reference to be attached to the object.
attachObject_childReference :: Lens.Lens' AttachObject ObjectReference
attachObject_childReference = Lens.lens (\AttachObject' {childReference} -> childReference) (\s@AttachObject' {} a -> s {childReference = a} :: AttachObject)

-- | The link name with which the child object is attached to the parent.
attachObject_linkName :: Lens.Lens' AttachObject Core.Text
attachObject_linkName = Lens.lens (\AttachObject' {linkName} -> linkName) (\s@AttachObject' {} a -> s {linkName = a} :: AttachObject)

instance Core.AWSRequest AttachObject where
  type AWSResponse AttachObject = AttachObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachObjectResponse'
            Core.<$> (x Core..?> "AttachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachObject

instance Core.NFData AttachObject

instance Core.ToHeaders AttachObject where
  toHeaders AttachObject' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON AttachObject where
  toJSON AttachObject' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ParentReference" Core..= parentReference),
            Core.Just ("ChildReference" Core..= childReference),
            Core.Just ("LinkName" Core..= linkName)
          ]
      )

instance Core.ToPath AttachObject where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/attach"

instance Core.ToQuery AttachObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachObjectResponse' smart constructor.
data AttachObjectResponse = AttachObjectResponse'
  { -- | The attached @ObjectIdentifier@, which is the child @ObjectIdentifier@.
    attachedObjectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedObjectIdentifier', 'attachObjectResponse_attachedObjectIdentifier' - The attached @ObjectIdentifier@, which is the child @ObjectIdentifier@.
--
-- 'httpStatus', 'attachObjectResponse_httpStatus' - The response's http status code.
newAttachObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachObjectResponse
newAttachObjectResponse pHttpStatus_ =
  AttachObjectResponse'
    { attachedObjectIdentifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attached @ObjectIdentifier@, which is the child @ObjectIdentifier@.
attachObjectResponse_attachedObjectIdentifier :: Lens.Lens' AttachObjectResponse (Core.Maybe Core.Text)
attachObjectResponse_attachedObjectIdentifier = Lens.lens (\AttachObjectResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@AttachObjectResponse' {} a -> s {attachedObjectIdentifier = a} :: AttachObjectResponse)

-- | The response's http status code.
attachObjectResponse_httpStatus :: Lens.Lens' AttachObjectResponse Core.Int
attachObjectResponse_httpStatus = Lens.lens (\AttachObjectResponse' {httpStatus} -> httpStatus) (\s@AttachObjectResponse' {} a -> s {httpStatus = a} :: AttachObjectResponse)

instance Core.NFData AttachObjectResponse
