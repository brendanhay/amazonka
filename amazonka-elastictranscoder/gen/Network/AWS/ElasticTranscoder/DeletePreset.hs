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
-- Module      : Network.AWS.ElasticTranscoder.DeletePreset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The DeletePreset operation removes a preset that you\'ve added in an AWS
-- region.
--
-- You can\'t delete the default presets that are included with Elastic
-- Transcoder.
module Network.AWS.ElasticTranscoder.DeletePreset
  ( -- * Creating a Request
    DeletePreset (..),
    newDeletePreset,

    -- * Request Lenses
    deletePreset_id,

    -- * Destructuring the Response
    DeletePresetResponse (..),
    newDeletePresetResponse,

    -- * Response Lenses
    deletePresetResponse_httpStatus,
  )
where

import Network.AWS.ElasticTranscoder.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The @DeletePresetRequest@ structure.
--
-- /See:/ 'newDeletePreset' smart constructor.
data DeletePreset = DeletePreset'
  { -- | The identifier of the preset for which you want to get detailed
    -- information.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePreset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deletePreset_id' - The identifier of the preset for which you want to get detailed
-- information.
newDeletePreset ::
  -- | 'id'
  Prelude.Text ->
  DeletePreset
newDeletePreset pId_ = DeletePreset' {id = pId_}

-- | The identifier of the preset for which you want to get detailed
-- information.
deletePreset_id :: Lens.Lens' DeletePreset Prelude.Text
deletePreset_id = Lens.lens (\DeletePreset' {id} -> id) (\s@DeletePreset' {} a -> s {id = a} :: DeletePreset)

instance Prelude.AWSRequest DeletePreset where
  type Rs DeletePreset = DeletePresetResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeletePresetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeletePreset

instance Prelude.NFData DeletePreset

instance Prelude.ToHeaders DeletePreset where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePreset where
  toPath DeletePreset' {..} =
    Prelude.mconcat
      ["/2012-09-25/presets/", Prelude.toBS id]

instance Prelude.ToQuery DeletePreset where
  toQuery = Prelude.const Prelude.mempty

-- | The @DeletePresetResponse@ structure.
--
-- /See:/ 'newDeletePresetResponse' smart constructor.
data DeletePresetResponse = DeletePresetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeletePresetResponse
newDeletePresetResponse pHttpStatus_ =
  DeletePresetResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deletePresetResponse_httpStatus :: Lens.Lens' DeletePresetResponse Prelude.Int
deletePresetResponse_httpStatus = Lens.lens (\DeletePresetResponse' {httpStatus} -> httpStatus) (\s@DeletePresetResponse' {} a -> s {httpStatus = a} :: DeletePresetResponse)

instance Prelude.NFData DeletePresetResponse
