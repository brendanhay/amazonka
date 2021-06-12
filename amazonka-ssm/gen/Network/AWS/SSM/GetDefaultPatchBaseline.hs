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
-- Module      : Network.AWS.SSM.GetDefaultPatchBaseline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the default patch baseline. Note that Systems Manager supports
-- creating multiple default patch baselines. For example, you can create a
-- default patch baseline for each operating system.
--
-- If you do not specify an operating system value, the default patch
-- baseline for Windows is returned.
module Network.AWS.SSM.GetDefaultPatchBaseline
  ( -- * Creating a Request
    GetDefaultPatchBaseline (..),
    newGetDefaultPatchBaseline,

    -- * Request Lenses
    getDefaultPatchBaseline_operatingSystem,

    -- * Destructuring the Response
    GetDefaultPatchBaselineResponse (..),
    newGetDefaultPatchBaselineResponse,

    -- * Response Lenses
    getDefaultPatchBaselineResponse_baselineId,
    getDefaultPatchBaselineResponse_operatingSystem,
    getDefaultPatchBaselineResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetDefaultPatchBaseline' smart constructor.
data GetDefaultPatchBaseline = GetDefaultPatchBaseline'
  { -- | Returns the default patch baseline for the specified operating system.
    operatingSystem :: Core.Maybe OperatingSystem
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDefaultPatchBaseline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operatingSystem', 'getDefaultPatchBaseline_operatingSystem' - Returns the default patch baseline for the specified operating system.
newGetDefaultPatchBaseline ::
  GetDefaultPatchBaseline
newGetDefaultPatchBaseline =
  GetDefaultPatchBaseline'
    { operatingSystem =
        Core.Nothing
    }

-- | Returns the default patch baseline for the specified operating system.
getDefaultPatchBaseline_operatingSystem :: Lens.Lens' GetDefaultPatchBaseline (Core.Maybe OperatingSystem)
getDefaultPatchBaseline_operatingSystem = Lens.lens (\GetDefaultPatchBaseline' {operatingSystem} -> operatingSystem) (\s@GetDefaultPatchBaseline' {} a -> s {operatingSystem = a} :: GetDefaultPatchBaseline)

instance Core.AWSRequest GetDefaultPatchBaseline where
  type
    AWSResponse GetDefaultPatchBaseline =
      GetDefaultPatchBaselineResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDefaultPatchBaselineResponse'
            Core.<$> (x Core..?> "BaselineId")
            Core.<*> (x Core..?> "OperatingSystem")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDefaultPatchBaseline

instance Core.NFData GetDefaultPatchBaseline

instance Core.ToHeaders GetDefaultPatchBaseline where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetDefaultPatchBaseline" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDefaultPatchBaseline where
  toJSON GetDefaultPatchBaseline' {..} =
    Core.object
      ( Core.catMaybes
          [ ("OperatingSystem" Core..=)
              Core.<$> operatingSystem
          ]
      )

instance Core.ToPath GetDefaultPatchBaseline where
  toPath = Core.const "/"

instance Core.ToQuery GetDefaultPatchBaseline where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDefaultPatchBaselineResponse' smart constructor.
data GetDefaultPatchBaselineResponse = GetDefaultPatchBaselineResponse'
  { -- | The ID of the default patch baseline.
    baselineId :: Core.Maybe Core.Text,
    -- | The operating system for the returned patch baseline.
    operatingSystem :: Core.Maybe OperatingSystem,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDefaultPatchBaselineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineId', 'getDefaultPatchBaselineResponse_baselineId' - The ID of the default patch baseline.
--
-- 'operatingSystem', 'getDefaultPatchBaselineResponse_operatingSystem' - The operating system for the returned patch baseline.
--
-- 'httpStatus', 'getDefaultPatchBaselineResponse_httpStatus' - The response's http status code.
newGetDefaultPatchBaselineResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDefaultPatchBaselineResponse
newGetDefaultPatchBaselineResponse pHttpStatus_ =
  GetDefaultPatchBaselineResponse'
    { baselineId =
        Core.Nothing,
      operatingSystem = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the default patch baseline.
getDefaultPatchBaselineResponse_baselineId :: Lens.Lens' GetDefaultPatchBaselineResponse (Core.Maybe Core.Text)
getDefaultPatchBaselineResponse_baselineId = Lens.lens (\GetDefaultPatchBaselineResponse' {baselineId} -> baselineId) (\s@GetDefaultPatchBaselineResponse' {} a -> s {baselineId = a} :: GetDefaultPatchBaselineResponse)

-- | The operating system for the returned patch baseline.
getDefaultPatchBaselineResponse_operatingSystem :: Lens.Lens' GetDefaultPatchBaselineResponse (Core.Maybe OperatingSystem)
getDefaultPatchBaselineResponse_operatingSystem = Lens.lens (\GetDefaultPatchBaselineResponse' {operatingSystem} -> operatingSystem) (\s@GetDefaultPatchBaselineResponse' {} a -> s {operatingSystem = a} :: GetDefaultPatchBaselineResponse)

-- | The response's http status code.
getDefaultPatchBaselineResponse_httpStatus :: Lens.Lens' GetDefaultPatchBaselineResponse Core.Int
getDefaultPatchBaselineResponse_httpStatus = Lens.lens (\GetDefaultPatchBaselineResponse' {httpStatus} -> httpStatus) (\s@GetDefaultPatchBaselineResponse' {} a -> s {httpStatus = a} :: GetDefaultPatchBaselineResponse)

instance Core.NFData GetDefaultPatchBaselineResponse
