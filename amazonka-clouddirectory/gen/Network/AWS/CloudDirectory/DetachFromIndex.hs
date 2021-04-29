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
-- Module      : Network.AWS.CloudDirectory.DetachFromIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified object from the specified index.
module Network.AWS.CloudDirectory.DetachFromIndex
  ( -- * Creating a Request
    DetachFromIndex (..),
    newDetachFromIndex,

    -- * Request Lenses
    detachFromIndex_directoryArn,
    detachFromIndex_indexReference,
    detachFromIndex_targetReference,

    -- * Destructuring the Response
    DetachFromIndexResponse (..),
    newDetachFromIndexResponse,

    -- * Response Lenses
    detachFromIndexResponse_detachedObjectIdentifier,
    detachFromIndexResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachFromIndex' smart constructor.
data DetachFromIndex = DetachFromIndex'
  { -- | The Amazon Resource Name (ARN) of the directory the index and object
    -- exist in.
    directoryArn :: Prelude.Text,
    -- | A reference to the index object.
    indexReference :: ObjectReference,
    -- | A reference to the object being detached from the index.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachFromIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'detachFromIndex_directoryArn' - The Amazon Resource Name (ARN) of the directory the index and object
-- exist in.
--
-- 'indexReference', 'detachFromIndex_indexReference' - A reference to the index object.
--
-- 'targetReference', 'detachFromIndex_targetReference' - A reference to the object being detached from the index.
newDetachFromIndex ::
  -- | 'directoryArn'
  Prelude.Text ->
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  DetachFromIndex
newDetachFromIndex
  pDirectoryArn_
  pIndexReference_
  pTargetReference_ =
    DetachFromIndex'
      { directoryArn = pDirectoryArn_,
        indexReference = pIndexReference_,
        targetReference = pTargetReference_
      }

-- | The Amazon Resource Name (ARN) of the directory the index and object
-- exist in.
detachFromIndex_directoryArn :: Lens.Lens' DetachFromIndex Prelude.Text
detachFromIndex_directoryArn = Lens.lens (\DetachFromIndex' {directoryArn} -> directoryArn) (\s@DetachFromIndex' {} a -> s {directoryArn = a} :: DetachFromIndex)

-- | A reference to the index object.
detachFromIndex_indexReference :: Lens.Lens' DetachFromIndex ObjectReference
detachFromIndex_indexReference = Lens.lens (\DetachFromIndex' {indexReference} -> indexReference) (\s@DetachFromIndex' {} a -> s {indexReference = a} :: DetachFromIndex)

-- | A reference to the object being detached from the index.
detachFromIndex_targetReference :: Lens.Lens' DetachFromIndex ObjectReference
detachFromIndex_targetReference = Lens.lens (\DetachFromIndex' {targetReference} -> targetReference) (\s@DetachFromIndex' {} a -> s {targetReference = a} :: DetachFromIndex)

instance Prelude.AWSRequest DetachFromIndex where
  type Rs DetachFromIndex = DetachFromIndexResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DetachFromIndexResponse'
            Prelude.<$> (x Prelude..?> "DetachedObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DetachFromIndex

instance Prelude.NFData DetachFromIndex

instance Prelude.ToHeaders DetachFromIndex where
  toHeaders DetachFromIndex' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON DetachFromIndex where
  toJSON DetachFromIndex' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IndexReference" Prelude..= indexReference),
            Prelude.Just
              ("TargetReference" Prelude..= targetReference)
          ]
      )

instance Prelude.ToPath DetachFromIndex where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/index/detach"

instance Prelude.ToQuery DetachFromIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDetachFromIndexResponse' smart constructor.
data DetachFromIndexResponse = DetachFromIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was detached from the index.
    detachedObjectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DetachFromIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'detachedObjectIdentifier', 'detachFromIndexResponse_detachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
--
-- 'httpStatus', 'detachFromIndexResponse_httpStatus' - The response's http status code.
newDetachFromIndexResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DetachFromIndexResponse
newDetachFromIndexResponse pHttpStatus_ =
  DetachFromIndexResponse'
    { detachedObjectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
detachFromIndexResponse_detachedObjectIdentifier :: Lens.Lens' DetachFromIndexResponse (Prelude.Maybe Prelude.Text)
detachFromIndexResponse_detachedObjectIdentifier = Lens.lens (\DetachFromIndexResponse' {detachedObjectIdentifier} -> detachedObjectIdentifier) (\s@DetachFromIndexResponse' {} a -> s {detachedObjectIdentifier = a} :: DetachFromIndexResponse)

-- | The response's http status code.
detachFromIndexResponse_httpStatus :: Lens.Lens' DetachFromIndexResponse Prelude.Int
detachFromIndexResponse_httpStatus = Lens.lens (\DetachFromIndexResponse' {httpStatus} -> httpStatus) (\s@DetachFromIndexResponse' {} a -> s {httpStatus = a} :: DetachFromIndexResponse)

instance Prelude.NFData DetachFromIndexResponse
