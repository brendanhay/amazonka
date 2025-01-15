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
-- Module      : Amazonka.StorageGateway.DescribeCachediSCSIVolumes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of the gateway volumes specified in the request.
-- This operation is only supported in the cached volume gateway types.
--
-- The list of gateway volumes in the request must be from one gateway. In
-- the response, Storage Gateway returns volume information sorted by
-- volume Amazon Resource Name (ARN).
module Amazonka.StorageGateway.DescribeCachediSCSIVolumes
  ( -- * Creating a Request
    DescribeCachediSCSIVolumes (..),
    newDescribeCachediSCSIVolumes,

    -- * Request Lenses
    describeCachediSCSIVolumes_volumeARNs,

    -- * Destructuring the Response
    DescribeCachediSCSIVolumesResponse (..),
    newDescribeCachediSCSIVolumesResponse,

    -- * Response Lenses
    describeCachediSCSIVolumesResponse_cachediSCSIVolumes,
    describeCachediSCSIVolumesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDescribeCachediSCSIVolumes' smart constructor.
data DescribeCachediSCSIVolumes = DescribeCachediSCSIVolumes'
  { -- | An array of strings where each string represents the Amazon Resource
    -- Name (ARN) of a cached volume. All of the specified cached volumes must
    -- be from the same gateway. Use ListVolumes to get volume ARNs for a
    -- gateway.
    volumeARNs :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCachediSCSIVolumes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'volumeARNs', 'describeCachediSCSIVolumes_volumeARNs' - An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a cached volume. All of the specified cached volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
newDescribeCachediSCSIVolumes ::
  DescribeCachediSCSIVolumes
newDescribeCachediSCSIVolumes =
  DescribeCachediSCSIVolumes'
    { volumeARNs =
        Prelude.mempty
    }

-- | An array of strings where each string represents the Amazon Resource
-- Name (ARN) of a cached volume. All of the specified cached volumes must
-- be from the same gateway. Use ListVolumes to get volume ARNs for a
-- gateway.
describeCachediSCSIVolumes_volumeARNs :: Lens.Lens' DescribeCachediSCSIVolumes [Prelude.Text]
describeCachediSCSIVolumes_volumeARNs = Lens.lens (\DescribeCachediSCSIVolumes' {volumeARNs} -> volumeARNs) (\s@DescribeCachediSCSIVolumes' {} a -> s {volumeARNs = a} :: DescribeCachediSCSIVolumes) Prelude.. Lens.coerced

instance Core.AWSRequest DescribeCachediSCSIVolumes where
  type
    AWSResponse DescribeCachediSCSIVolumes =
      DescribeCachediSCSIVolumesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCachediSCSIVolumesResponse'
            Prelude.<$> ( x
                            Data..?> "CachediSCSIVolumes"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeCachediSCSIVolumes where
  hashWithSalt _salt DescribeCachediSCSIVolumes' {..} =
    _salt `Prelude.hashWithSalt` volumeARNs

instance Prelude.NFData DescribeCachediSCSIVolumes where
  rnf DescribeCachediSCSIVolumes' {..} =
    Prelude.rnf volumeARNs

instance Data.ToHeaders DescribeCachediSCSIVolumes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeCachediSCSIVolumes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeCachediSCSIVolumes where
  toJSON DescribeCachediSCSIVolumes' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("VolumeARNs" Data..= volumeARNs)]
      )

instance Data.ToPath DescribeCachediSCSIVolumes where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeCachediSCSIVolumes where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeCachediSCSIVolumesResponse' smart constructor.
data DescribeCachediSCSIVolumesResponse = DescribeCachediSCSIVolumesResponse'
  { -- | An array of objects where each object contains metadata about one cached
    -- volume.
    cachediSCSIVolumes :: Prelude.Maybe [CachediSCSIVolume],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCachediSCSIVolumesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachediSCSIVolumes', 'describeCachediSCSIVolumesResponse_cachediSCSIVolumes' - An array of objects where each object contains metadata about one cached
-- volume.
--
-- 'httpStatus', 'describeCachediSCSIVolumesResponse_httpStatus' - The response's http status code.
newDescribeCachediSCSIVolumesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeCachediSCSIVolumesResponse
newDescribeCachediSCSIVolumesResponse pHttpStatus_ =
  DescribeCachediSCSIVolumesResponse'
    { cachediSCSIVolumes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects where each object contains metadata about one cached
-- volume.
describeCachediSCSIVolumesResponse_cachediSCSIVolumes :: Lens.Lens' DescribeCachediSCSIVolumesResponse (Prelude.Maybe [CachediSCSIVolume])
describeCachediSCSIVolumesResponse_cachediSCSIVolumes = Lens.lens (\DescribeCachediSCSIVolumesResponse' {cachediSCSIVolumes} -> cachediSCSIVolumes) (\s@DescribeCachediSCSIVolumesResponse' {} a -> s {cachediSCSIVolumes = a} :: DescribeCachediSCSIVolumesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeCachediSCSIVolumesResponse_httpStatus :: Lens.Lens' DescribeCachediSCSIVolumesResponse Prelude.Int
describeCachediSCSIVolumesResponse_httpStatus = Lens.lens (\DescribeCachediSCSIVolumesResponse' {httpStatus} -> httpStatus) (\s@DescribeCachediSCSIVolumesResponse' {} a -> s {httpStatus = a} :: DescribeCachediSCSIVolumesResponse)

instance
  Prelude.NFData
    DescribeCachediSCSIVolumesResponse
  where
  rnf DescribeCachediSCSIVolumesResponse' {..} =
    Prelude.rnf cachediSCSIVolumes `Prelude.seq`
      Prelude.rnf httpStatus
