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
-- Module      : Amazonka.DataSync.DescribeLocationSmb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata, such as the path and user information about an SMB
-- location.
module Amazonka.DataSync.DescribeLocationSmb
  ( -- * Creating a Request
    DescribeLocationSmb (..),
    newDescribeLocationSmb,

    -- * Request Lenses
    describeLocationSmb_locationArn,

    -- * Destructuring the Response
    DescribeLocationSmbResponse (..),
    newDescribeLocationSmbResponse,

    -- * Response Lenses
    describeLocationSmbResponse_agentArns,
    describeLocationSmbResponse_creationTime,
    describeLocationSmbResponse_domain,
    describeLocationSmbResponse_locationArn,
    describeLocationSmbResponse_locationUri,
    describeLocationSmbResponse_mountOptions,
    describeLocationSmbResponse_user,
    describeLocationSmbResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationSmbRequest
--
-- /See:/ 'newDescribeLocationSmb' smart constructor.
data DescribeLocationSmb = DescribeLocationSmb'
  { -- | The Amazon Resource Name (ARN) of the SMB location to describe.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationSmb' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationSmb_locationArn' - The Amazon Resource Name (ARN) of the SMB location to describe.
newDescribeLocationSmb ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationSmb
newDescribeLocationSmb pLocationArn_ =
  DescribeLocationSmb' {locationArn = pLocationArn_}

-- | The Amazon Resource Name (ARN) of the SMB location to describe.
describeLocationSmb_locationArn :: Lens.Lens' DescribeLocationSmb Prelude.Text
describeLocationSmb_locationArn = Lens.lens (\DescribeLocationSmb' {locationArn} -> locationArn) (\s@DescribeLocationSmb' {} a -> s {locationArn = a} :: DescribeLocationSmb)

instance Core.AWSRequest DescribeLocationSmb where
  type
    AWSResponse DescribeLocationSmb =
      DescribeLocationSmbResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationSmbResponse'
            Prelude.<$> (x Data..?> "AgentArns")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "Domain")
            Prelude.<*> (x Data..?> "LocationArn")
            Prelude.<*> (x Data..?> "LocationUri")
            Prelude.<*> (x Data..?> "MountOptions")
            Prelude.<*> (x Data..?> "User")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeLocationSmb where
  hashWithSalt _salt DescribeLocationSmb' {..} =
    _salt `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationSmb where
  rnf DescribeLocationSmb' {..} =
    Prelude.rnf locationArn

instance Data.ToHeaders DescribeLocationSmb where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeLocationSmb" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeLocationSmb where
  toJSON DescribeLocationSmb' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Data..= locationArn)]
      )

instance Data.ToPath DescribeLocationSmb where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeLocationSmb where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationSmbResponse
--
-- /See:/ 'newDescribeLocationSmbResponse' smart constructor.
data DescribeLocationSmbResponse = DescribeLocationSmbResponse'
  { -- | The Amazon Resource Name (ARN) of the source SMB file system location
    -- that is created.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The time that the SMB location was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the Windows domain that the SMB server belongs to.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SMB location that was described.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The URL of the source SMB location that was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The mount options that are available for DataSync to use to access an
    -- SMB location.
    mountOptions :: Prelude.Maybe SmbMountOptions,
    -- | The user who can mount the share, has the permissions to access files
    -- and folders in the SMB share.
    user :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationSmbResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'describeLocationSmbResponse_agentArns' - The Amazon Resource Name (ARN) of the source SMB file system location
-- that is created.
--
-- 'creationTime', 'describeLocationSmbResponse_creationTime' - The time that the SMB location was created.
--
-- 'domain', 'describeLocationSmbResponse_domain' - The name of the Windows domain that the SMB server belongs to.
--
-- 'locationArn', 'describeLocationSmbResponse_locationArn' - The Amazon Resource Name (ARN) of the SMB location that was described.
--
-- 'locationUri', 'describeLocationSmbResponse_locationUri' - The URL of the source SMB location that was described.
--
-- 'mountOptions', 'describeLocationSmbResponse_mountOptions' - The mount options that are available for DataSync to use to access an
-- SMB location.
--
-- 'user', 'describeLocationSmbResponse_user' - The user who can mount the share, has the permissions to access files
-- and folders in the SMB share.
--
-- 'httpStatus', 'describeLocationSmbResponse_httpStatus' - The response's http status code.
newDescribeLocationSmbResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationSmbResponse
newDescribeLocationSmbResponse pHttpStatus_ =
  DescribeLocationSmbResponse'
    { agentArns =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      domain = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      mountOptions = Prelude.Nothing,
      user = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the source SMB file system location
-- that is created.
describeLocationSmbResponse_agentArns :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationSmbResponse_agentArns = Lens.lens (\DescribeLocationSmbResponse' {agentArns} -> agentArns) (\s@DescribeLocationSmbResponse' {} a -> s {agentArns = a} :: DescribeLocationSmbResponse) Prelude.. Lens.mapping Lens.coerced

-- | The time that the SMB location was created.
describeLocationSmbResponse_creationTime :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationSmbResponse_creationTime = Lens.lens (\DescribeLocationSmbResponse' {creationTime} -> creationTime) (\s@DescribeLocationSmbResponse' {} a -> s {creationTime = a} :: DescribeLocationSmbResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the Windows domain that the SMB server belongs to.
describeLocationSmbResponse_domain :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe Prelude.Text)
describeLocationSmbResponse_domain = Lens.lens (\DescribeLocationSmbResponse' {domain} -> domain) (\s@DescribeLocationSmbResponse' {} a -> s {domain = a} :: DescribeLocationSmbResponse)

-- | The Amazon Resource Name (ARN) of the SMB location that was described.
describeLocationSmbResponse_locationArn :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe Prelude.Text)
describeLocationSmbResponse_locationArn = Lens.lens (\DescribeLocationSmbResponse' {locationArn} -> locationArn) (\s@DescribeLocationSmbResponse' {} a -> s {locationArn = a} :: DescribeLocationSmbResponse)

-- | The URL of the source SMB location that was described.
describeLocationSmbResponse_locationUri :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe Prelude.Text)
describeLocationSmbResponse_locationUri = Lens.lens (\DescribeLocationSmbResponse' {locationUri} -> locationUri) (\s@DescribeLocationSmbResponse' {} a -> s {locationUri = a} :: DescribeLocationSmbResponse)

-- | The mount options that are available for DataSync to use to access an
-- SMB location.
describeLocationSmbResponse_mountOptions :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe SmbMountOptions)
describeLocationSmbResponse_mountOptions = Lens.lens (\DescribeLocationSmbResponse' {mountOptions} -> mountOptions) (\s@DescribeLocationSmbResponse' {} a -> s {mountOptions = a} :: DescribeLocationSmbResponse)

-- | The user who can mount the share, has the permissions to access files
-- and folders in the SMB share.
describeLocationSmbResponse_user :: Lens.Lens' DescribeLocationSmbResponse (Prelude.Maybe Prelude.Text)
describeLocationSmbResponse_user = Lens.lens (\DescribeLocationSmbResponse' {user} -> user) (\s@DescribeLocationSmbResponse' {} a -> s {user = a} :: DescribeLocationSmbResponse)

-- | The response's http status code.
describeLocationSmbResponse_httpStatus :: Lens.Lens' DescribeLocationSmbResponse Prelude.Int
describeLocationSmbResponse_httpStatus = Lens.lens (\DescribeLocationSmbResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationSmbResponse' {} a -> s {httpStatus = a} :: DescribeLocationSmbResponse)

instance Prelude.NFData DescribeLocationSmbResponse where
  rnf DescribeLocationSmbResponse' {..} =
    Prelude.rnf agentArns `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf domain `Prelude.seq`
          Prelude.rnf locationArn `Prelude.seq`
            Prelude.rnf locationUri `Prelude.seq`
              Prelude.rnf mountOptions `Prelude.seq`
                Prelude.rnf user `Prelude.seq`
                  Prelude.rnf httpStatus
