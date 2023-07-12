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
-- Module      : Amazonka.WorkSpaces.DescribeClientBranding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified client branding. Client branding allows you to
-- customize the log in page of various device types for your users. You
-- can add your company logo, the support email address, support link, link
-- to reset password, and a custom message for users trying to sign in.
--
-- Only device types that have branding information configured will be
-- shown in the response.
module Amazonka.WorkSpaces.DescribeClientBranding
  ( -- * Creating a Request
    DescribeClientBranding (..),
    newDescribeClientBranding,

    -- * Request Lenses
    describeClientBranding_resourceId,

    -- * Destructuring the Response
    DescribeClientBrandingResponse (..),
    newDescribeClientBrandingResponse,

    -- * Response Lenses
    describeClientBrandingResponse_deviceTypeAndroid,
    describeClientBrandingResponse_deviceTypeIos,
    describeClientBrandingResponse_deviceTypeLinux,
    describeClientBrandingResponse_deviceTypeOsx,
    describeClientBrandingResponse_deviceTypeWeb,
    describeClientBrandingResponse_deviceTypeWindows,
    describeClientBrandingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newDescribeClientBranding' smart constructor.
data DescribeClientBranding = DescribeClientBranding'
  { -- | The directory identifier of the WorkSpace for which you want to view
    -- client branding information.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientBranding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'describeClientBranding_resourceId' - The directory identifier of the WorkSpace for which you want to view
-- client branding information.
newDescribeClientBranding ::
  -- | 'resourceId'
  Prelude.Text ->
  DescribeClientBranding
newDescribeClientBranding pResourceId_ =
  DescribeClientBranding' {resourceId = pResourceId_}

-- | The directory identifier of the WorkSpace for which you want to view
-- client branding information.
describeClientBranding_resourceId :: Lens.Lens' DescribeClientBranding Prelude.Text
describeClientBranding_resourceId = Lens.lens (\DescribeClientBranding' {resourceId} -> resourceId) (\s@DescribeClientBranding' {} a -> s {resourceId = a} :: DescribeClientBranding)

instance Core.AWSRequest DescribeClientBranding where
  type
    AWSResponse DescribeClientBranding =
      DescribeClientBrandingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeClientBrandingResponse'
            Prelude.<$> (x Data..?> "DeviceTypeAndroid")
            Prelude.<*> (x Data..?> "DeviceTypeIos")
            Prelude.<*> (x Data..?> "DeviceTypeLinux")
            Prelude.<*> (x Data..?> "DeviceTypeOsx")
            Prelude.<*> (x Data..?> "DeviceTypeWeb")
            Prelude.<*> (x Data..?> "DeviceTypeWindows")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeClientBranding where
  hashWithSalt _salt DescribeClientBranding' {..} =
    _salt `Prelude.hashWithSalt` resourceId

instance Prelude.NFData DescribeClientBranding where
  rnf DescribeClientBranding' {..} =
    Prelude.rnf resourceId

instance Data.ToHeaders DescribeClientBranding where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "WorkspacesService.DescribeClientBranding" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeClientBranding where
  toJSON DescribeClientBranding' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceId" Data..= resourceId)]
      )

instance Data.ToPath DescribeClientBranding where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeClientBranding where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeClientBrandingResponse' smart constructor.
data DescribeClientBrandingResponse = DescribeClientBrandingResponse'
  { -- | The branding information for Android devices.
    deviceTypeAndroid :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information for iOS devices.
    deviceTypeIos :: Prelude.Maybe IosClientBrandingAttributes,
    -- | The branding information for Linux devices.
    deviceTypeLinux :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information for macOS devices.
    deviceTypeOsx :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information for Web access.
    deviceTypeWeb :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information for Windows devices.
    deviceTypeWindows :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeClientBrandingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceTypeAndroid', 'describeClientBrandingResponse_deviceTypeAndroid' - The branding information for Android devices.
--
-- 'deviceTypeIos', 'describeClientBrandingResponse_deviceTypeIos' - The branding information for iOS devices.
--
-- 'deviceTypeLinux', 'describeClientBrandingResponse_deviceTypeLinux' - The branding information for Linux devices.
--
-- 'deviceTypeOsx', 'describeClientBrandingResponse_deviceTypeOsx' - The branding information for macOS devices.
--
-- 'deviceTypeWeb', 'describeClientBrandingResponse_deviceTypeWeb' - The branding information for Web access.
--
-- 'deviceTypeWindows', 'describeClientBrandingResponse_deviceTypeWindows' - The branding information for Windows devices.
--
-- 'httpStatus', 'describeClientBrandingResponse_httpStatus' - The response's http status code.
newDescribeClientBrandingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeClientBrandingResponse
newDescribeClientBrandingResponse pHttpStatus_ =
  DescribeClientBrandingResponse'
    { deviceTypeAndroid =
        Prelude.Nothing,
      deviceTypeIos = Prelude.Nothing,
      deviceTypeLinux = Prelude.Nothing,
      deviceTypeOsx = Prelude.Nothing,
      deviceTypeWeb = Prelude.Nothing,
      deviceTypeWindows = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The branding information for Android devices.
describeClientBrandingResponse_deviceTypeAndroid :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeAndroid = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeAndroid = a} :: DescribeClientBrandingResponse)

-- | The branding information for iOS devices.
describeClientBrandingResponse_deviceTypeIos :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe IosClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeIos = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeIos} -> deviceTypeIos) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeIos = a} :: DescribeClientBrandingResponse)

-- | The branding information for Linux devices.
describeClientBrandingResponse_deviceTypeLinux :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeLinux = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeLinux} -> deviceTypeLinux) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeLinux = a} :: DescribeClientBrandingResponse)

-- | The branding information for macOS devices.
describeClientBrandingResponse_deviceTypeOsx :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeOsx = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeOsx} -> deviceTypeOsx) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeOsx = a} :: DescribeClientBrandingResponse)

-- | The branding information for Web access.
describeClientBrandingResponse_deviceTypeWeb :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeWeb = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeWeb} -> deviceTypeWeb) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeWeb = a} :: DescribeClientBrandingResponse)

-- | The branding information for Windows devices.
describeClientBrandingResponse_deviceTypeWindows :: Lens.Lens' DescribeClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
describeClientBrandingResponse_deviceTypeWindows = Lens.lens (\DescribeClientBrandingResponse' {deviceTypeWindows} -> deviceTypeWindows) (\s@DescribeClientBrandingResponse' {} a -> s {deviceTypeWindows = a} :: DescribeClientBrandingResponse)

-- | The response's http status code.
describeClientBrandingResponse_httpStatus :: Lens.Lens' DescribeClientBrandingResponse Prelude.Int
describeClientBrandingResponse_httpStatus = Lens.lens (\DescribeClientBrandingResponse' {httpStatus} -> httpStatus) (\s@DescribeClientBrandingResponse' {} a -> s {httpStatus = a} :: DescribeClientBrandingResponse)

instance
  Prelude.NFData
    DescribeClientBrandingResponse
  where
  rnf DescribeClientBrandingResponse' {..} =
    Prelude.rnf deviceTypeAndroid
      `Prelude.seq` Prelude.rnf deviceTypeIos
      `Prelude.seq` Prelude.rnf deviceTypeLinux
      `Prelude.seq` Prelude.rnf deviceTypeOsx
      `Prelude.seq` Prelude.rnf deviceTypeWeb
      `Prelude.seq` Prelude.rnf deviceTypeWindows
      `Prelude.seq` Prelude.rnf httpStatus
