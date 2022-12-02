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
-- Module      : Amazonka.EMR.DescribeStudio
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details for the specified Amazon EMR Studio including ID, Name,
-- VPC, Studio access URL, and so on.
module Amazonka.EMR.DescribeStudio
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EMR.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStudioResponse'
            Prelude.<$> (x Data..?> "Studio")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStudio where
  hashWithSalt _salt DescribeStudio' {..} =
    _salt `Prelude.hashWithSalt` studioId

instance Prelude.NFData DescribeStudio where
  rnf DescribeStudio' {..} = Prelude.rnf studioId

instance Data.ToHeaders DescribeStudio where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ElasticMapReduce.DescribeStudio" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStudio where
  toJSON DescribeStudio' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("StudioId" Data..= studioId)]
      )

instance Data.ToPath DescribeStudio where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStudio where
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

instance Prelude.NFData DescribeStudioResponse where
  rnf DescribeStudioResponse' {..} =
    Prelude.rnf studio
      `Prelude.seq` Prelude.rnf httpStatus
