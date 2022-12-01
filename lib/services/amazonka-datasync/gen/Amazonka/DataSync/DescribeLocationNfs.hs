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
-- Module      : Amazonka.DataSync.DescribeLocationNfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the path information, about an NFS location.
module Amazonka.DataSync.DescribeLocationNfs
  ( -- * Creating a Request
    DescribeLocationNfs (..),
    newDescribeLocationNfs,

    -- * Request Lenses
    describeLocationNfs_locationArn,

    -- * Destructuring the Response
    DescribeLocationNfsResponse (..),
    newDescribeLocationNfsResponse,

    -- * Response Lenses
    describeLocationNfsResponse_onPremConfig,
    describeLocationNfsResponse_mountOptions,
    describeLocationNfsResponse_locationArn,
    describeLocationNfsResponse_locationUri,
    describeLocationNfsResponse_creationTime,
    describeLocationNfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationNfsRequest
--
-- /See:/ 'newDescribeLocationNfs' smart constructor.
data DescribeLocationNfs = DescribeLocationNfs'
  { -- | The Amazon Resource Name (ARN) of the NFS location to describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationNfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationNfs_locationArn' - The Amazon Resource Name (ARN) of the NFS location to describe.
newDescribeLocationNfs ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationNfs
newDescribeLocationNfs pLocationArn_ =
  DescribeLocationNfs' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the NFS location to describe.
describeLocationNfs_locationArn :: Lens.Lens' DescribeLocationNfs Prelude.Text
describeLocationNfs_locationArn = Lens.lens (\DescribeLocationNfs' {locationArn} -> locationArn) (\s@DescribeLocationNfs' {} a -> s {locationArn = a} :: DescribeLocationNfs)

instance Core.AWSRequest DescribeLocationNfs where
  type
    AWSResponse DescribeLocationNfs =
      DescribeLocationNfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationNfsResponse'
            Prelude.<$> (x Core..?> "OnPremConfig")
            Prelude.<*> (x Core..?> "MountOptions")
            Prelude.<*> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationNfs where
  hashWithSalt _salt DescribeLocationNfs' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationNfs where
  rnf DescribeLocationNfs' {..} =
    Prelude.rnf locationArn

instance Core.ToHeaders DescribeLocationNfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.DescribeLocationNfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLocationNfs where
  toJSON DescribeLocationNfs' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DescribeLocationNfs where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocationNfs where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationNfsResponse
--
-- /See:/ 'newDescribeLocationNfsResponse' smart constructor.
data DescribeLocationNfsResponse = DescribeLocationNfsResponse'
  { onPremConfig :: Prelude.Maybe OnPremConfig,
    -- | The NFS mount options that DataSync used to mount your NFS share.
    mountOptions :: Prelude.Maybe NfsMountOptions,
    -- | The Amazon Resource Name (ARN) of the NFS location that was described.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The URL of the source NFS location that was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The time that the NFS location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationNfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'onPremConfig', 'describeLocationNfsResponse_onPremConfig' - Undocumented member.
--
-- 'mountOptions', 'describeLocationNfsResponse_mountOptions' - The NFS mount options that DataSync used to mount your NFS share.
--
-- 'locationArn', 'describeLocationNfsResponse_locationArn' - The Amazon Resource Name (ARN) of the NFS location that was described.
--
-- 'locationUri', 'describeLocationNfsResponse_locationUri' - The URL of the source NFS location that was described.
--
-- 'creationTime', 'describeLocationNfsResponse_creationTime' - The time that the NFS location was created.
--
-- 'httpStatus', 'describeLocationNfsResponse_httpStatus' - The response's http status code.
newDescribeLocationNfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationNfsResponse
newDescribeLocationNfsResponse pHttpStatus_ =
  DescribeLocationNfsResponse'
    { onPremConfig =
        Prelude.Nothing,
      mountOptions = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeLocationNfsResponse_onPremConfig :: Lens.Lens' DescribeLocationNfsResponse (Prelude.Maybe OnPremConfig)
describeLocationNfsResponse_onPremConfig = Lens.lens (\DescribeLocationNfsResponse' {onPremConfig} -> onPremConfig) (\s@DescribeLocationNfsResponse' {} a -> s {onPremConfig = a} :: DescribeLocationNfsResponse)

-- | The NFS mount options that DataSync used to mount your NFS share.
describeLocationNfsResponse_mountOptions :: Lens.Lens' DescribeLocationNfsResponse (Prelude.Maybe NfsMountOptions)
describeLocationNfsResponse_mountOptions = Lens.lens (\DescribeLocationNfsResponse' {mountOptions} -> mountOptions) (\s@DescribeLocationNfsResponse' {} a -> s {mountOptions = a} :: DescribeLocationNfsResponse)

-- | The Amazon Resource Name (ARN) of the NFS location that was described.
describeLocationNfsResponse_locationArn :: Lens.Lens' DescribeLocationNfsResponse (Prelude.Maybe Prelude.Text)
describeLocationNfsResponse_locationArn = Lens.lens (\DescribeLocationNfsResponse' {locationArn} -> locationArn) (\s@DescribeLocationNfsResponse' {} a -> s {locationArn = a} :: DescribeLocationNfsResponse)

-- | The URL of the source NFS location that was described.
describeLocationNfsResponse_locationUri :: Lens.Lens' DescribeLocationNfsResponse (Prelude.Maybe Prelude.Text)
describeLocationNfsResponse_locationUri = Lens.lens (\DescribeLocationNfsResponse' {locationUri} -> locationUri) (\s@DescribeLocationNfsResponse' {} a -> s {locationUri = a} :: DescribeLocationNfsResponse)

-- | The time that the NFS location was created.
describeLocationNfsResponse_creationTime :: Lens.Lens' DescribeLocationNfsResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationNfsResponse_creationTime = Lens.lens (\DescribeLocationNfsResponse' {creationTime} -> creationTime) (\s@DescribeLocationNfsResponse' {} a -> s {creationTime = a} :: DescribeLocationNfsResponse) Prelude.. Lens.mapping Core._Time

-- | The response's http status code.
describeLocationNfsResponse_httpStatus :: Lens.Lens' DescribeLocationNfsResponse Prelude.Int
describeLocationNfsResponse_httpStatus = Lens.lens (\DescribeLocationNfsResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationNfsResponse' {} a -> s {httpStatus = a} :: DescribeLocationNfsResponse)

instance Prelude.NFData DescribeLocationNfsResponse where
  rnf DescribeLocationNfsResponse' {..} =
    Prelude.rnf onPremConfig
      `Prelude.seq` Prelude.rnf mountOptions
      `Prelude.seq` Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf httpStatus
