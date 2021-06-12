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
-- Module      : Network.AWS.CloudTrail.GetTrail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns settings information for a specified trail.
module Network.AWS.CloudTrail.GetTrail
  ( -- * Creating a Request
    GetTrail (..),
    newGetTrail,

    -- * Request Lenses
    getTrail_name,

    -- * Destructuring the Response
    GetTrailResponse (..),
    newGetTrailResponse,

    -- * Response Lenses
    getTrailResponse_trail,
    getTrailResponse_httpStatus,
  )
where

import Network.AWS.CloudTrail.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTrail' smart constructor.
data GetTrail = GetTrail'
  { -- | The name or the Amazon Resource Name (ARN) of the trail for which you
    -- want to retrieve settings information.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTrail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getTrail_name' - The name or the Amazon Resource Name (ARN) of the trail for which you
-- want to retrieve settings information.
newGetTrail ::
  -- | 'name'
  Core.Text ->
  GetTrail
newGetTrail pName_ = GetTrail' {name = pName_}

-- | The name or the Amazon Resource Name (ARN) of the trail for which you
-- want to retrieve settings information.
getTrail_name :: Lens.Lens' GetTrail Core.Text
getTrail_name = Lens.lens (\GetTrail' {name} -> name) (\s@GetTrail' {} a -> s {name = a} :: GetTrail)

instance Core.AWSRequest GetTrail where
  type AWSResponse GetTrail = GetTrailResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTrailResponse'
            Core.<$> (x Core..?> "Trail")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetTrail

instance Core.NFData GetTrail

instance Core.ToHeaders GetTrail where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetTrail" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetTrail where
  toJSON GetTrail' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetTrail where
  toPath = Core.const "/"

instance Core.ToQuery GetTrail where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetTrailResponse' smart constructor.
data GetTrailResponse = GetTrailResponse'
  { trail :: Core.Maybe Trail,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetTrailResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'trail', 'getTrailResponse_trail' - Undocumented member.
--
-- 'httpStatus', 'getTrailResponse_httpStatus' - The response's http status code.
newGetTrailResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetTrailResponse
newGetTrailResponse pHttpStatus_ =
  GetTrailResponse'
    { trail = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getTrailResponse_trail :: Lens.Lens' GetTrailResponse (Core.Maybe Trail)
getTrailResponse_trail = Lens.lens (\GetTrailResponse' {trail} -> trail) (\s@GetTrailResponse' {} a -> s {trail = a} :: GetTrailResponse)

-- | The response's http status code.
getTrailResponse_httpStatus :: Lens.Lens' GetTrailResponse Core.Int
getTrailResponse_httpStatus = Lens.lens (\GetTrailResponse' {httpStatus} -> httpStatus) (\s@GetTrailResponse' {} a -> s {httpStatus = a} :: GetTrailResponse)

instance Core.NFData GetTrailResponse
