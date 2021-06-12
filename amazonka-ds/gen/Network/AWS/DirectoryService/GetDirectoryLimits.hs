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
-- Module      : Network.AWS.DirectoryService.GetDirectoryLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Obtains directory limit information for the current Region.
module Network.AWS.DirectoryService.GetDirectoryLimits
  ( -- * Creating a Request
    GetDirectoryLimits (..),
    newGetDirectoryLimits,

    -- * Destructuring the Response
    GetDirectoryLimitsResponse (..),
    newGetDirectoryLimitsResponse,

    -- * Response Lenses
    getDirectoryLimitsResponse_directoryLimits,
    getDirectoryLimitsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the GetDirectoryLimits operation.
--
-- /See:/ 'newGetDirectoryLimits' smart constructor.
data GetDirectoryLimits = GetDirectoryLimits'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDirectoryLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetDirectoryLimits ::
  GetDirectoryLimits
newGetDirectoryLimits = GetDirectoryLimits'

instance Core.AWSRequest GetDirectoryLimits where
  type
    AWSResponse GetDirectoryLimits =
      GetDirectoryLimitsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDirectoryLimitsResponse'
            Core.<$> (x Core..?> "DirectoryLimits")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDirectoryLimits

instance Core.NFData GetDirectoryLimits

instance Core.ToHeaders GetDirectoryLimits where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.GetDirectoryLimits" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDirectoryLimits where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetDirectoryLimits where
  toPath = Core.const "/"

instance Core.ToQuery GetDirectoryLimits where
  toQuery = Core.const Core.mempty

-- | Contains the results of the GetDirectoryLimits operation.
--
-- /See:/ 'newGetDirectoryLimitsResponse' smart constructor.
data GetDirectoryLimitsResponse = GetDirectoryLimitsResponse'
  { -- | A DirectoryLimits object that contains the directory limits for the
    -- current rRegion.
    directoryLimits :: Core.Maybe DirectoryLimits,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDirectoryLimitsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryLimits', 'getDirectoryLimitsResponse_directoryLimits' - A DirectoryLimits object that contains the directory limits for the
-- current rRegion.
--
-- 'httpStatus', 'getDirectoryLimitsResponse_httpStatus' - The response's http status code.
newGetDirectoryLimitsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDirectoryLimitsResponse
newGetDirectoryLimitsResponse pHttpStatus_ =
  GetDirectoryLimitsResponse'
    { directoryLimits =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A DirectoryLimits object that contains the directory limits for the
-- current rRegion.
getDirectoryLimitsResponse_directoryLimits :: Lens.Lens' GetDirectoryLimitsResponse (Core.Maybe DirectoryLimits)
getDirectoryLimitsResponse_directoryLimits = Lens.lens (\GetDirectoryLimitsResponse' {directoryLimits} -> directoryLimits) (\s@GetDirectoryLimitsResponse' {} a -> s {directoryLimits = a} :: GetDirectoryLimitsResponse)

-- | The response's http status code.
getDirectoryLimitsResponse_httpStatus :: Lens.Lens' GetDirectoryLimitsResponse Core.Int
getDirectoryLimitsResponse_httpStatus = Lens.lens (\GetDirectoryLimitsResponse' {httpStatus} -> httpStatus) (\s@GetDirectoryLimitsResponse' {} a -> s {httpStatus = a} :: GetDirectoryLimitsResponse)

instance Core.NFData GetDirectoryLimitsResponse
