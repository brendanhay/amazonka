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
-- Module      : Network.AWS.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be
-- detached from the parent is specified by the link name.
module Network.AWS.CloudDirectory.DetachObject
  ( -- * Creating a Request
    DetachObject (..),
    newDetachObject,

    -- * Request Lenses
    detachObject_directoryArn,
    detachObject_parentReference,
    detachObject_linkName,

    -- * Destructuring the Response
    DetachObjectResponse (..),
    newDetachObjectResponse,

    -- * Response Lenses
    detachObjectResponse_detachedObjectIdentifier,
    detachObjectResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachObject' smart constructor.
data DetachObject = DetachObject'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where objects reside. For more information, see arns.
    directoryArn :: Core.Text,
    -- | The parent reference from which the object with the specified link name
    -- is detached.
    parentReference :: ObjectReference,
    -- | The link name associated with the object that needs to be detached.
    linkName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'detachObject_directoryArn' - The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
--
-- 'parentReference', 'detachObject_parentReference' - The parent reference from which the object with the specified link name
-- is detached.
--
-- 'linkName', 'detachObject_linkName' - The link name associated with the object that needs to be detached.
newDetachObject ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'linkName'
  Core.Text ->
  DetachObject
newDetachObject
  pDirectoryArn_
  pParentReference_
  pLinkName_ =
    DetachObject'
      { directoryArn = pDirectoryArn_,
        parentReference = pParentReference_,
        linkName = pLinkName_
      }

-- | The Amazon Resource Name (ARN) that is associated with the Directory
-- where objects reside. For more information, see arns.
detachObject_directoryArn :: Lens.Lens' DetachObject Core.Text
detachObject_directoryArn = Lens.lens (\DetachObject' {directoryArn} -> directoryArn) (\s@DetachObject' {} a -> s {directoryArn = a} :: DetachObject)

-- | The parent reference from which the object with the specified link name
-- is detached.
detachObject_parentReference :: Lens.Lens' DetachObject ObjectReference
detachObject_parentReference = Lens.lens (\DetachObject' {parentReference} -> parentReference) (\s@DetachObject' {} a -> s {parentReference = a} :: DetachObject)

-- | The link name associated with the object that needs to be detached.
detachObject_linkName :: Lens.Lens' DetachObject Core.Text
detachObject_linkName = Lens.lens (\DetachObject' {linkName} -> linkName) (\s@DetachObject' {} a -> s {linkName = a} :: DetachObject)

instance Core.AWSRequest DetachObject where
  type AWSResponse DetachObject = DetachObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachObjectResponse'
            Core.<$> (x Core..?> "DetachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DetachObject

instance Core.NFData DetachObject

instance Core.ToHeaders DetachObject where
  toHeaders DetachObject' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON DetachObject where
  toJSON DetachObject' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("ParentReference" Core..= parentReference),
            Core.Just ("LinkName" Core..= linkName)
          ]
      )

instance Core.ToPath DetachObject where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/object/detach"

instance Core.ToQuery DetachObject where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDetachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { -- | The @ObjectIdentifier@ that was detached from the object.
    detachedObjectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detachedObjectIdentifier', 'detachObjectResponse_detachedObjectIdentifier' - The @ObjectIdentifier@ that was detached from the object.
--
-- 'httpStatus', 'detachObjectResponse_httpStatus' - The response's http status code.
newDetachObjectResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DetachObjectResponse
newDetachObjectResponse pHttpStatus_ =
  DetachObjectResponse'
    { detachedObjectIdentifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ that was detached from the object.
detachObjectResponse_detachedObjectIdentifier :: Lens.Lens' DetachObjectResponse (Core.Maybe Core.Text)
detachObjectResponse_detachedObjectIdentifier = Lens.lens (\DetachObjectResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@DetachObjectResponse' {} a -> s {detachedObjectIdentifier = a} :: DetachObjectResponse)

-- | The response's http status code.
detachObjectResponse_httpStatus :: Lens.Lens' DetachObjectResponse Core.Int
detachObjectResponse_httpStatus = Lens.lens (\DetachObjectResponse' {httpStatus} -> httpStatus) (\s@DetachObjectResponse' {} a -> s {httpStatus = a} :: DetachObjectResponse)

instance Core.NFData DetachObjectResponse
