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
-- Module      : Network.AWS.DirectoryService.RemoveRegion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops all replication and removes the domain controllers from the
-- specified Region. You cannot remove the primary Region with this
-- operation. Instead, use the @DeleteDirectory@ API.
module Network.AWS.DirectoryService.RemoveRegion
  ( -- * Creating a Request
    RemoveRegion (..),
    newRemoveRegion,

    -- * Request Lenses
    removeRegion_directoryId,

    -- * Destructuring the Response
    RemoveRegionResponse (..),
    newRemoveRegionResponse,

    -- * Response Lenses
    removeRegionResponse_httpStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRemoveRegion' smart constructor.
data RemoveRegion = RemoveRegion'
  { -- | The identifier of the directory for which you want to remove Region
    -- replication.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'removeRegion_directoryId' - The identifier of the directory for which you want to remove Region
-- replication.
newRemoveRegion ::
  -- | 'directoryId'
  Prelude.Text ->
  RemoveRegion
newRemoveRegion pDirectoryId_ =
  RemoveRegion' {directoryId = pDirectoryId_}

-- | The identifier of the directory for which you want to remove Region
-- replication.
removeRegion_directoryId :: Lens.Lens' RemoveRegion Prelude.Text
removeRegion_directoryId = Lens.lens (\RemoveRegion' {directoryId} -> directoryId) (\s@RemoveRegion' {} a -> s {directoryId = a} :: RemoveRegion)

instance Prelude.AWSRequest RemoveRegion where
  type Rs RemoveRegion = RemoveRegionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          RemoveRegionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RemoveRegion

instance Prelude.NFData RemoveRegion

instance Prelude.ToHeaders RemoveRegion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DirectoryService_20150416.RemoveRegion" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON RemoveRegion where
  toJSON RemoveRegion' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DirectoryId" Prelude..= directoryId)
          ]
      )

instance Prelude.ToPath RemoveRegion where
  toPath = Prelude.const "/"

instance Prelude.ToQuery RemoveRegion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRemoveRegionResponse' smart constructor.
data RemoveRegionResponse = RemoveRegionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'RemoveRegionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'removeRegionResponse_httpStatus' - The response's http status code.
newRemoveRegionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RemoveRegionResponse
newRemoveRegionResponse pHttpStatus_ =
  RemoveRegionResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
removeRegionResponse_httpStatus :: Lens.Lens' RemoveRegionResponse Prelude.Int
removeRegionResponse_httpStatus = Lens.lens (\RemoveRegionResponse' {httpStatus} -> httpStatus) (\s@RemoveRegionResponse' {} a -> s {httpStatus = a} :: RemoveRegionResponse)

instance Prelude.NFData RemoveRegionResponse
