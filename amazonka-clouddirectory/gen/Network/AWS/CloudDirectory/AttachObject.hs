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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachObject' smart constructor.
data AttachObject = AttachObject'
  { -- | Amazon Resource Name (ARN) that is associated with the Directory where
    -- both objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The parent object reference.
    parentReference :: ObjectReference,
    -- | The child object reference to be attached to the object.
    childReference :: ObjectReference,
    -- | The link name with which the child object is attached to the parent.
    linkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'childReference'
  ObjectReference ->
  -- | 'linkName'
  Prelude.Text ->
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
attachObject_directoryArn :: Lens.Lens' AttachObject Prelude.Text
attachObject_directoryArn = Lens.lens (\AttachObject' {directoryArn} -> directoryArn) (\s@AttachObject' {} a -> s {directoryArn = a} :: AttachObject)

-- | The parent object reference.
attachObject_parentReference :: Lens.Lens' AttachObject ObjectReference
attachObject_parentReference = Lens.lens (\AttachObject' {parentReference} -> parentReference) (\s@AttachObject' {} a -> s {parentReference = a} :: AttachObject)

-- | The child object reference to be attached to the object.
attachObject_childReference :: Lens.Lens' AttachObject ObjectReference
attachObject_childReference = Lens.lens (\AttachObject' {childReference} -> childReference) (\s@AttachObject' {} a -> s {childReference = a} :: AttachObject)

-- | The link name with which the child object is attached to the parent.
attachObject_linkName :: Lens.Lens' AttachObject Prelude.Text
attachObject_linkName = Lens.lens (\AttachObject' {linkName} -> linkName) (\s@AttachObject' {} a -> s {linkName = a} :: AttachObject)

instance Prelude.AWSRequest AttachObject where
  type Rs AttachObject = AttachObjectResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachObjectResponse'
            Prelude.<$> (x Prelude..?> "AttachedObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachObject

instance Prelude.NFData AttachObject

instance Prelude.ToHeaders AttachObject where
  toHeaders AttachObject' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON AttachObject where
  toJSON AttachObject' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParentReference" Prelude..= parentReference),
            Prelude.Just
              ("ChildReference" Prelude..= childReference),
            Prelude.Just ("LinkName" Prelude..= linkName)
          ]
      )

instance Prelude.ToPath AttachObject where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/attach"

instance Prelude.ToQuery AttachObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachObjectResponse' smart constructor.
data AttachObjectResponse = AttachObjectResponse'
  { -- | The attached @ObjectIdentifier@, which is the child @ObjectIdentifier@.
    attachedObjectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  AttachObjectResponse
newAttachObjectResponse pHttpStatus_ =
  AttachObjectResponse'
    { attachedObjectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attached @ObjectIdentifier@, which is the child @ObjectIdentifier@.
attachObjectResponse_attachedObjectIdentifier :: Lens.Lens' AttachObjectResponse (Prelude.Maybe Prelude.Text)
attachObjectResponse_attachedObjectIdentifier = Lens.lens (\AttachObjectResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@AttachObjectResponse' {} a -> s {attachedObjectIdentifier = a} :: AttachObjectResponse)

-- | The response's http status code.
attachObjectResponse_httpStatus :: Lens.Lens' AttachObjectResponse Prelude.Int
attachObjectResponse_httpStatus = Lens.lens (\AttachObjectResponse' {httpStatus} -> httpStatus) (\s@AttachObjectResponse' {} a -> s {httpStatus = a} :: AttachObjectResponse)

instance Prelude.NFData AttachObjectResponse
