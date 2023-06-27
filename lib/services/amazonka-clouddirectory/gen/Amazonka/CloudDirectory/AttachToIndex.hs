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
-- Module      : Amazonka.CloudDirectory.AttachToIndex
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified object to the specified index.
module Amazonka.CloudDirectory.AttachToIndex
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

import Amazonka.CloudDirectory.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { -- | The Amazon Resource Name (ARN) of the directory where the object and
    -- index exist.
    directoryArn :: Prelude.Text,
    -- | A reference to the index that you are attaching the object to.
    indexReference :: ObjectReference,
    -- | A reference to the object that you are attaching to the index.
    targetReference :: ObjectReference
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
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
attachToIndex_directoryArn :: Lens.Lens' AttachToIndex Prelude.Text
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
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachToIndexResponse'
            Prelude.<$> (x Data..?> "AttachedObjectIdentifier")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AttachToIndex where
  hashWithSalt _salt AttachToIndex' {..} =
    _salt
      `Prelude.hashWithSalt` directoryArn
      `Prelude.hashWithSalt` indexReference
      `Prelude.hashWithSalt` targetReference

instance Prelude.NFData AttachToIndex where
  rnf AttachToIndex' {..} =
    Prelude.rnf directoryArn
      `Prelude.seq` Prelude.rnf indexReference
      `Prelude.seq` Prelude.rnf targetReference

instance Data.ToHeaders AttachToIndex where
  toHeaders AttachToIndex' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Data.=# directoryArn]

instance Data.ToJSON AttachToIndex where
  toJSON AttachToIndex' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IndexReference" Data..= indexReference),
            Prelude.Just
              ("TargetReference" Data..= targetReference)
          ]
      )

instance Data.ToPath AttachToIndex where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/index/attach"

instance Data.ToQuery AttachToIndex where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  AttachToIndexResponse
newAttachToIndexResponse pHttpStatus_ =
  AttachToIndexResponse'
    { attachedObjectIdentifier =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
attachToIndexResponse_attachedObjectIdentifier :: Lens.Lens' AttachToIndexResponse (Prelude.Maybe Prelude.Text)
attachToIndexResponse_attachedObjectIdentifier = Lens.lens (\AttachToIndexResponse' {attachedObjectIdentifier} -> attachedObjectIdentifier) (\s@AttachToIndexResponse' {} a -> s {attachedObjectIdentifier = a} :: AttachToIndexResponse)

-- | The response's http status code.
attachToIndexResponse_httpStatus :: Lens.Lens' AttachToIndexResponse Prelude.Int
attachToIndexResponse_httpStatus = Lens.lens (\AttachToIndexResponse' {httpStatus} -> httpStatus) (\s@AttachToIndexResponse' {} a -> s {httpStatus = a} :: AttachToIndexResponse)

instance Prelude.NFData AttachToIndexResponse where
  rnf AttachToIndexResponse' {..} =
    Prelude.rnf attachedObjectIdentifier
      `Prelude.seq` Prelude.rnf httpStatus
