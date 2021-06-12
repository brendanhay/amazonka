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
-- Module      : Network.AWS.CloudDirectory.AttachToIndex
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified object to the specified index.
module Network.AWS.CloudDirectory.AttachToIndex
  ( -- * Creating a Request
    AttachToIndex (..),
    newAttachToIndex,

    -- * Request Lenses
    attachToIndex_directoryArn,
    attachToIndex_indexReference,
    attachToIndex_targetReference,

    -- * Destructuring the Response
    AttachToIndexResponse (..),
    newAttachToIndexResponse,

    -- * Response Lenses
    attachToIndexResponse_attachedObjectIdentifier,
    attachToIndexResponse_httpStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { -- | The Amazon Resource Name (ARN) of the directory where the object and
    -- index exist.
    directoryArn :: Core.Text,
    -- | A reference to the index that you are attaching the object to.
    indexReference :: ObjectReference,
    -- | A reference to the object that you are attaching to the index.
    targetReference :: ObjectReference
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachToIndex' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'attachToIndex_directoryArn' - The Amazon Resource Name (ARN) of the directory where the object and
-- index exist.
--
-- 'indexReference', 'attachToIndex_indexReference' - A reference to the index that you are attaching the object to.
--
-- 'targetReference', 'attachToIndex_targetReference' - A reference to the object that you are attaching to the index.
newAttachToIndex ::
  -- | 'directoryArn'
  Core.Text ->
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  AttachToIndex
newAttachToIndex
  pDirectoryArn_
  pIndexReference_
  pTargetReference_ =
    AttachToIndex'
      { directoryArn = pDirectoryArn_,
        indexReference = pIndexReference_,
        targetReference = pTargetReference_
      }

-- | The Amazon Resource Name (ARN) of the directory where the object and
-- index exist.
attachToIndex_directoryArn :: Lens.Lens' AttachToIndex Core.Text
attachToIndex_directoryArn = Lens.lens (\AttachToIndex' {directoryArn} -> directoryArn) (\s@AttachToIndex' {} a -> s {directoryArn = a} :: AttachToIndex)

-- | A reference to the index that you are attaching the object to.
attachToIndex_indexReference :: Lens.Lens' AttachToIndex ObjectReference
attachToIndex_indexReference = Lens.lens (\AttachToIndex' {indexReference} -> indexReference) (\s@AttachToIndex' {} a -> s {indexReference = a} :: AttachToIndex)

-- | A reference to the object that you are attaching to the index.
attachToIndex_targetReference :: Lens.Lens' AttachToIndex ObjectReference
attachToIndex_targetReference = Lens.lens (\AttachToIndex' {targetReference} -> targetReference) (\s@AttachToIndex' {} a -> s {targetReference = a} :: AttachToIndex)

instance Core.AWSRequest AttachToIndex where
  type
    AWSResponse AttachToIndex =
      AttachToIndexResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachToIndexResponse'
            Core.<$> (x Core..?> "AttachedObjectIdentifier")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AttachToIndex

instance Core.NFData AttachToIndex

instance Core.ToHeaders AttachToIndex where
  toHeaders AttachToIndex' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON AttachToIndex where
  toJSON AttachToIndex' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("IndexReference" Core..= indexReference),
            Core.Just
              ("TargetReference" Core..= targetReference)
          ]
      )

instance Core.ToPath AttachToIndex where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/index/attach"

instance Core.ToQuery AttachToIndex where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newAttachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachToIndexResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attachedObjectIdentifier', 'attachToIndexResponse_attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
--
-- 'httpStatus', 'attachToIndexResponse_httpStatus' - The response's http status code.
newAttachToIndexResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AttachToIndexResponse
newAttachToIndexResponse pHttpStatus_ =
  AttachToIndexResponse'
    { attachedObjectIdentifier =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
attachToIndexResponse_attachedObjectIdentifier :: Lens.Lens' AttachToIndexResponse (Core.Maybe Core.Text)
attachToIndexResponse_attachedObjectIdentifier = Lens.lens (\AttachToIndexResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@AttachToIndexResponse' {} a -> s {attachedObjectIdentifier = a} :: AttachToIndexResponse)

-- | The response's http status code.
attachToIndexResponse_httpStatus :: Lens.Lens' AttachToIndexResponse Core.Int
attachToIndexResponse_httpStatus = Lens.lens (\AttachToIndexResponse' {httpStatus} -> httpStatus) (\s@AttachToIndexResponse' {} a -> s {httpStatus = a} :: AttachToIndexResponse)

instance Core.NFData AttachToIndexResponse
