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
-- Module      : Amazonka.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be
-- detached from the parent is specified by the link name.
module Amazonka.CloudDirectory.DetachObject
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDetachObject' smart constructor.
data DetachObject = DetachObject'
  { -- | The Amazon Resource Name (ARN) that is associated with the Directory
    -- where objects reside. For more information, see arns.
    directoryArn :: Prelude.Text,
    -- | The parent reference from which the object with the specified link name
    -- is detached.
    parentReference :: ObjectReference,
    -- | The link name associated with the object that needs to be detached.
    linkName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'linkName'
  Prelude.Text ->
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
detachObject_directoryArn :: Lens.Lens' DetachObject Prelude.Text
detachObject_directoryArn = Lens.lens (\DetachObject' {directoryArn} -> directoryArn) (\s@DetachObject' {} a -> s {directoryArn = a} :: DetachObject)

-- | The parent reference from which the object with the specified link name
-- is detached.
detachObject_parentReference :: Lens.Lens' DetachObject ObjectReference
detachObject_parentReference = Lens.lens (\DetachObject' {parentReference} -> parentReference) (\s@DetachObject' {} a -> s {parentReference = a} :: DetachObject)

-- | The link name associated with the object that needs to be detached.
detachObject_linkName :: Lens.Lens' DetachObject Prelude.Text
detachObject_linkName = Lens.lens (\DetachObject' {linkName} -> linkName) (\s@DetachObject' {} a -> s {linkName = a} :: DetachObject)

instance Core.AWSRequest DetachObject where
  type AWSResponse DetachObject = DetachObjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachObjectResponse'
            Prelude.<$> (x Data..?> "DetachedObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachObject where
  hashWithSalt _salt DetachObject' {..} =
    _salt
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` parentReference
      `Prelude.hashWithSalt` linkName

instance Prelude.NFData DetachObject where
  rnf DetachObject' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf parentReference
      `Prelude.seq` Prelude.rnf linkName

instance Data.ToHeaders DetachObject where
  toHeaders DetachObject' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON DetachObject where
  toJSON DetachObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ParentReference" Data..= parentReference),
            Prelude.Just ("LinkName" Data..= linkName)
          ]
      )

instance Data.ToPath DetachObject where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/object/detach"

instance Data.ToQuery DetachObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { -- | The @ObjectIdentifier@ that was detached from the object.
    detachedObjectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DetachObjectResponse
newDetachObjectResponse pHttpStatus_ =
  DetachObjectResponse'
    { detachedObjectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ that was detached from the object.
detachObjectResponse_detachedObjectIdentifier :: Lens.Lens' DetachObjectResponse (Prelude.Maybe Prelude.Text)
detachObjectResponse_detachedObjectIdentifier = Lens.lens (\DetachObjectResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@DetachObjectResponse' {} a -> s {detachedObjectIdentifier = a} :: DetachObjectResponse)

-- | The response's http status code.
detachObjectResponse_httpStatus :: Lens.Lens' DetachObjectResponse Prelude.Int
detachObjectResponse_httpStatus = Lens.lens (\DetachObjectResponse' {httpStatus} -> httpStatus) (\s@DetachObjectResponse' {} a -> s {httpStatus = a} :: DetachObjectResponse)

instance Prelude.NFData DetachObjectResponse where
  rnf DetachObjectResponse' {..} =
    Prelude.rnf detachedObjectIdentifier
      `Prelude.seq` Prelude.rnf httpStatus
