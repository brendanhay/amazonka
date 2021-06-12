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
-- Module      : Network.AWS.ElasticBeanstalk.DeletePlatformVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified version of a custom platform.
module Network.AWS.ElasticBeanstalk.DeletePlatformVersion
  ( -- * Creating a Request
    DeletePlatformVersion (..),
    newDeletePlatformVersion,

    -- * Request Lenses
    deletePlatformVersion_platformArn,

    -- * Destructuring the Response
    DeletePlatformVersionResponse (..),
    newDeletePlatformVersionResponse,

    -- * Response Lenses
    deletePlatformVersionResponse_platformSummary,
    deletePlatformVersionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePlatformVersion' smart constructor.
data DeletePlatformVersion = DeletePlatformVersion'
  { -- | The ARN of the version of the custom platform.
    platformArn :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePlatformVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformArn', 'deletePlatformVersion_platformArn' - The ARN of the version of the custom platform.
newDeletePlatformVersion ::
  DeletePlatformVersion
newDeletePlatformVersion =
  DeletePlatformVersion' {platformArn = Core.Nothing}

-- | The ARN of the version of the custom platform.
deletePlatformVersion_platformArn :: Lens.Lens' DeletePlatformVersion (Core.Maybe Core.Text)
deletePlatformVersion_platformArn = Lens.lens (\DeletePlatformVersion' {platformArn} -> platformArn) (\s@DeletePlatformVersion' {} a -> s {platformArn = a} :: DeletePlatformVersion)

instance Core.AWSRequest DeletePlatformVersion where
  type
    AWSResponse DeletePlatformVersion =
      DeletePlatformVersionResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeletePlatformVersionResult"
      ( \s h x ->
          DeletePlatformVersionResponse'
            Core.<$> (x Core..@? "PlatformSummary")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeletePlatformVersion

instance Core.NFData DeletePlatformVersion

instance Core.ToHeaders DeletePlatformVersion where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeletePlatformVersion where
  toPath = Core.const "/"

instance Core.ToQuery DeletePlatformVersion where
  toQuery DeletePlatformVersion' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeletePlatformVersion" :: Core.ByteString),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "PlatformArn" Core.=: platformArn
      ]

-- | /See:/ 'newDeletePlatformVersionResponse' smart constructor.
data DeletePlatformVersionResponse = DeletePlatformVersionResponse'
  { -- | Detailed information about the version of the custom platform.
    platformSummary :: Core.Maybe PlatformSummary,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePlatformVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformSummary', 'deletePlatformVersionResponse_platformSummary' - Detailed information about the version of the custom platform.
--
-- 'httpStatus', 'deletePlatformVersionResponse_httpStatus' - The response's http status code.
newDeletePlatformVersionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeletePlatformVersionResponse
newDeletePlatformVersionResponse pHttpStatus_ =
  DeletePlatformVersionResponse'
    { platformSummary =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Detailed information about the version of the custom platform.
deletePlatformVersionResponse_platformSummary :: Lens.Lens' DeletePlatformVersionResponse (Core.Maybe PlatformSummary)
deletePlatformVersionResponse_platformSummary = Lens.lens (\DeletePlatformVersionResponse' {platformSummary} -> platformSummary) (\s@DeletePlatformVersionResponse' {} a -> s {platformSummary = a} :: DeletePlatformVersionResponse)

-- | The response's http status code.
deletePlatformVersionResponse_httpStatus :: Lens.Lens' DeletePlatformVersionResponse Core.Int
deletePlatformVersionResponse_httpStatus = Lens.lens (\DeletePlatformVersionResponse' {httpStatus} -> httpStatus) (\s@DeletePlatformVersionResponse' {} a -> s {httpStatus = a} :: DeletePlatformVersionResponse)

instance Core.NFData DeletePlatformVersionResponse
