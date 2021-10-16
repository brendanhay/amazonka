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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeStudio' smart constructor.
data DescribeStudio = DescribeStudio'
  { -- | The Amazon EMR Studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeStudio
newDescribeStudio pStudioId_ =
  DescribeStudio' {studioId = pStudioId_}

-- | The Amazon EMR Studio ID.
describeStudio_studioId :: Lens.Lens' DescribeStudio Prelude.Text
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
            Prelude.<$> (x Core..?> "Studio")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStudio

instance Prelude.NFData DescribeStudio

instance Core.ToHeaders DescribeStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ElasticMapReduce.DescribeStudio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeStudio where
  toJSON DescribeStudio' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("StudioId" Core..= studioId)]
      )

instance Core.ToPath DescribeStudio where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStudioResponse' smart constructor.
data DescribeStudioResponse = DescribeStudioResponse'
  { -- | The Amazon EMR Studio details.
    studio :: Prelude.Maybe Studio,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeStudioResponse
newDescribeStudioResponse pHttpStatus_ =
  DescribeStudioResponse'
    { studio = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon EMR Studio details.
describeStudioResponse_studio :: Lens.Lens' DescribeStudioResponse (Prelude.Maybe Studio)
describeStudioResponse_studio = Lens.lens (\DescribeStudioResponse' {studio} -> studio) (\s@DescribeStudioResponse' {} a -> s {studio = a} :: DescribeStudioResponse)

-- | The response's http status code.
describeStudioResponse_httpStatus :: Lens.Lens' DescribeStudioResponse Prelude.Int
describeStudioResponse_httpStatus = Lens.lens (\DescribeStudioResponse' {httpStatus} -> httpStatus) (\s@DescribeStudioResponse' {} a -> s {httpStatus = a} :: DescribeStudioResponse)

instance Prelude.NFData DescribeStudioResponse
