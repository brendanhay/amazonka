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
-- Module      : Network.AWS.EMR.DescribeStudio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details for the specified Amazon EMR Studio including ID, Name,
-- VPC, Studio access URL, and so on.
module Network.AWS.EMR.DescribeStudio
  ( -- * Creating a Request
    DescribeStudio (..),
    newDescribeStudio,

    -- * Request Lenses
    describeStudio_studioId,

    -- * Destructuring the Response
    DescribeStudioResponse (..),
    newDescribeStudioResponse,

    -- * Response Lenses
    describeStudioResponse_studio,
    describeStudioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStudio' smart constructor.
data DescribeStudio = DescribeStudio'
  { -- | The Amazon EMR Studio ID.
    studioId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studioId', 'describeStudio_studioId' - The Amazon EMR Studio ID.
newDescribeStudio ::
  -- | 'studioId'
  Core.Text ->
  DescribeStudio
newDescribeStudio pStudioId_ =
  DescribeStudio' {studioId = pStudioId_}

-- | The Amazon EMR Studio ID.
describeStudio_studioId :: Lens.Lens' DescribeStudio Core.Text
describeStudio_studioId = Lens.lens (\DescribeStudio' {studioId} -> studioId) (\s@DescribeStudio' {} a -> s {studioId = a} :: DescribeStudio)

instance Core.AWSRequest DescribeStudio where
  type
    AWSResponse DescribeStudio =
      DescribeStudioResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStudioResponse'
            Core.<$> (x Core..?> "Studio")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeStudio

instance Core.NFData DescribeStudio

instance Core.ToHeaders DescribeStudio where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeStudio" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeStudio where
  toJSON DescribeStudio' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("StudioId" Core..= studioId)]
      )

instance Core.ToPath DescribeStudio where
  toPath = Core.const "/"

instance Core.ToQuery DescribeStudio where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeStudioResponse' smart constructor.
data DescribeStudioResponse = DescribeStudioResponse'
  { -- | The Amazon EMR Studio details.
    studio :: Core.Maybe Studio,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studio', 'describeStudioResponse_studio' - The Amazon EMR Studio details.
--
-- 'httpStatus', 'describeStudioResponse_httpStatus' - The response's http status code.
newDescribeStudioResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeStudioResponse
newDescribeStudioResponse pHttpStatus_ =
  DescribeStudioResponse'
    { studio = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon EMR Studio details.
describeStudioResponse_studio :: Lens.Lens' DescribeStudioResponse (Core.Maybe Studio)
describeStudioResponse_studio = Lens.lens (\DescribeStudioResponse' {studio} -> studio) (\s@DescribeStudioResponse' {} a -> s {studio = a} :: DescribeStudioResponse)

-- | The response's http status code.
describeStudioResponse_httpStatus :: Lens.Lens' DescribeStudioResponse Core.Int
describeStudioResponse_httpStatus = Lens.lens (\DescribeStudioResponse' {httpStatus} -> httpStatus) (\s@DescribeStudioResponse' {} a -> s {httpStatus = a} :: DescribeStudioResponse)

instance Core.NFData DescribeStudioResponse
