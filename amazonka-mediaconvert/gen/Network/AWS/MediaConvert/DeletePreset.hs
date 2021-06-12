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
-- Module      : Network.AWS.MediaConvert.DeletePreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Permanently delete a preset you have created.
module Network.AWS.MediaConvert.DeletePreset
  ( -- * Creating a Request
    DeletePreset (..),
    newDeletePreset,

    -- * Request Lenses
    deletePreset_name,

    -- * Destructuring the Response
    DeletePresetResponse (..),
    newDeletePresetResponse,

    -- * Response Lenses
    deletePresetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePreset' smart constructor.
data DeletePreset = DeletePreset'
  { -- | The name of the preset to be deleted.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deletePreset_name' - The name of the preset to be deleted.
newDeletePreset ::
  -- | 'name'
  Core.Text ->
  DeletePreset
newDeletePreset pName_ = DeletePreset' {name = pName_}

-- | The name of the preset to be deleted.
deletePreset_name :: Lens.Lens' DeletePreset Core.Text
deletePreset_name = Lens.lens (\DeletePreset' {name} -> name) (\s@DeletePreset' {} a -> s {name = a} :: DeletePreset)

instance Core.AWSRequest DeletePreset where
  type AWSResponse DeletePreset = DeletePresetResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePresetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePreset

instance Core.NFData DeletePreset

instance Core.ToHeaders DeletePreset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeletePreset where
  toPath DeletePreset' {..} =
    Core.mconcat
      ["/2017-08-29/presets/", Core.toBS name]

instance Core.ToQuery DeletePreset where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePresetResponse' smart constructor.
data DeletePresetResponse = DeletePresetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePresetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deletePresetResponse_httpStatus' - The response's http status code.
newDeletePresetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeletePresetResponse
newDeletePresetResponse pHttpStatus_ =
  DeletePresetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePresetResponse_httpStatus :: Lens.Lens' DeletePresetResponse Core.Int
deletePresetResponse_httpStatus = Lens.lens (\DeletePresetResponse' {httpStatus} -> httpStatus) (\s@DeletePresetResponse' {} a -> s {httpStatus = a} :: DeletePresetResponse)

instance Core.NFData DeletePresetResponse
