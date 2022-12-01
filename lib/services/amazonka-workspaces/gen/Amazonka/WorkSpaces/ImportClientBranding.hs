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
-- Module      : Amazonka.WorkSpaces.ImportClientBranding
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Imports client branding. Client branding allows you to customize your
-- WorkSpace\'s client login portal. You can tailor your login portal
-- company logo, the support email address, support link, link to reset
-- password, and a custom message for users trying to sign in.
--
-- After you import client branding, the default branding experience for
-- the specified platform type is replaced with the imported experience
--
-- -   You must specify at least one platform type when importing client
--     branding.
--
-- -   You can import up to 6 MB of data with each request. If your request
--     exceeds this limit, you can import client branding for different
--     platform types using separate requests.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify only one
--     parameter for each platform type, but not both.
--
-- -   Imported data can take up to a minute to appear in the WorkSpaces
--     client.
module Amazonka.WorkSpaces.ImportClientBranding
  ( -- * Creating a Request
    ImportClientBranding (..),
    newImportClientBranding,

    -- * Request Lenses
    importClientBranding_deviceTypeAndroid,
    importClientBranding_deviceTypeLinux,
    importClientBranding_deviceTypeWeb,
    importClientBranding_deviceTypeOsx,
    importClientBranding_deviceTypeWindows,
    importClientBranding_deviceTypeIos,
    importClientBranding_resourceId,

    -- * Destructuring the Response
    ImportClientBrandingResponse (..),
    newImportClientBrandingResponse,

    -- * Response Lenses
    importClientBrandingResponse_deviceTypeAndroid,
    importClientBrandingResponse_deviceTypeLinux,
    importClientBrandingResponse_deviceTypeWeb,
    importClientBrandingResponse_deviceTypeOsx,
    importClientBrandingResponse_deviceTypeWindows,
    importClientBrandingResponse_deviceTypeIos,
    importClientBrandingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WorkSpaces.Types

-- | /See:/ 'newImportClientBranding' smart constructor.
data ImportClientBranding = ImportClientBranding'
  { -- | The branding information to import for Android devices.
    deviceTypeAndroid :: Prelude.Maybe DefaultImportClientBrandingAttributes,
    -- | The branding information to import for Linux devices.
    deviceTypeLinux :: Prelude.Maybe DefaultImportClientBrandingAttributes,
    -- | The branding information to import for web access.
    deviceTypeWeb :: Prelude.Maybe DefaultImportClientBrandingAttributes,
    -- | The branding information to import for macOS devices.
    deviceTypeOsx :: Prelude.Maybe DefaultImportClientBrandingAttributes,
    -- | The branding information to import for Windows devices.
    deviceTypeWindows :: Prelude.Maybe DefaultImportClientBrandingAttributes,
    -- | The branding information to import for iOS devices.
    deviceTypeIos :: Prelude.Maybe IosImportClientBrandingAttributes,
    -- | The directory identifier of the WorkSpace for which you want to import
    -- client branding.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportClientBranding' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceTypeAndroid', 'importClientBranding_deviceTypeAndroid' - The branding information to import for Android devices.
--
-- 'deviceTypeLinux', 'importClientBranding_deviceTypeLinux' - The branding information to import for Linux devices.
--
-- 'deviceTypeWeb', 'importClientBranding_deviceTypeWeb' - The branding information to import for web access.
--
-- 'deviceTypeOsx', 'importClientBranding_deviceTypeOsx' - The branding information to import for macOS devices.
--
-- 'deviceTypeWindows', 'importClientBranding_deviceTypeWindows' - The branding information to import for Windows devices.
--
-- 'deviceTypeIos', 'importClientBranding_deviceTypeIos' - The branding information to import for iOS devices.
--
-- 'resourceId', 'importClientBranding_resourceId' - The directory identifier of the WorkSpace for which you want to import
-- client branding.
newImportClientBranding ::
  -- | 'resourceId'
  Prelude.Text ->
  ImportClientBranding
newImportClientBranding pResourceId_ =
  ImportClientBranding'
    { deviceTypeAndroid =
        Prelude.Nothing,
      deviceTypeLinux = Prelude.Nothing,
      deviceTypeWeb = Prelude.Nothing,
      deviceTypeOsx = Prelude.Nothing,
      deviceTypeWindows = Prelude.Nothing,
      deviceTypeIos = Prelude.Nothing,
      resourceId = pResourceId_
    }

-- | The branding information to import for Android devices.
importClientBranding_deviceTypeAndroid :: Lens.Lens' ImportClientBranding (Prelude.Maybe DefaultImportClientBrandingAttributes)
importClientBranding_deviceTypeAndroid = Lens.lens (\ImportClientBranding' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@ImportClientBranding' {} a -> s {deviceTypeAndroid = a} :: ImportClientBranding)

-- | The branding information to import for Linux devices.
importClientBranding_deviceTypeLinux :: Lens.Lens' ImportClientBranding (Prelude.Maybe DefaultImportClientBrandingAttributes)
importClientBranding_deviceTypeLinux = Lens.lens (\ImportClientBranding' {deviceTypeLinux} -> deviceTypeLinux) (\s@ImportClientBranding' {} a -> s {deviceTypeLinux = a} :: ImportClientBranding)

-- | The branding information to import for web access.
importClientBranding_deviceTypeWeb :: Lens.Lens' ImportClientBranding (Prelude.Maybe DefaultImportClientBrandingAttributes)
importClientBranding_deviceTypeWeb = Lens.lens (\ImportClientBranding' {deviceTypeWeb} -> deviceTypeWeb) (\s@ImportClientBranding' {} a -> s {deviceTypeWeb = a} :: ImportClientBranding)

-- | The branding information to import for macOS devices.
importClientBranding_deviceTypeOsx :: Lens.Lens' ImportClientBranding (Prelude.Maybe DefaultImportClientBrandingAttributes)
importClientBranding_deviceTypeOsx = Lens.lens (\ImportClientBranding' {deviceTypeOsx} -> deviceTypeOsx) (\s@ImportClientBranding' {} a -> s {deviceTypeOsx = a} :: ImportClientBranding)

-- | The branding information to import for Windows devices.
importClientBranding_deviceTypeWindows :: Lens.Lens' ImportClientBranding (Prelude.Maybe DefaultImportClientBrandingAttributes)
importClientBranding_deviceTypeWindows = Lens.lens (\ImportClientBranding' {deviceTypeWindows} -> deviceTypeWindows) (\s@ImportClientBranding' {} a -> s {deviceTypeWindows = a} :: ImportClientBranding)

-- | The branding information to import for iOS devices.
importClientBranding_deviceTypeIos :: Lens.Lens' ImportClientBranding (Prelude.Maybe IosImportClientBrandingAttributes)
importClientBranding_deviceTypeIos = Lens.lens (\ImportClientBranding' {deviceTypeIos} -> deviceTypeIos) (\s@ImportClientBranding' {} a -> s {deviceTypeIos = a} :: ImportClientBranding)

-- | The directory identifier of the WorkSpace for which you want to import
-- client branding.
importClientBranding_resourceId :: Lens.Lens' ImportClientBranding Prelude.Text
importClientBranding_resourceId = Lens.lens (\ImportClientBranding' {resourceId} -> resourceId) (\s@ImportClientBranding' {} a -> s {resourceId = a} :: ImportClientBranding)

instance Core.AWSRequest ImportClientBranding where
  type
    AWSResponse ImportClientBranding =
      ImportClientBrandingResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportClientBrandingResponse'
            Prelude.<$> (x Core..?> "DeviceTypeAndroid")
            Prelude.<*> (x Core..?> "DeviceTypeLinux")
            Prelude.<*> (x Core..?> "DeviceTypeWeb")
            Prelude.<*> (x Core..?> "DeviceTypeOsx")
            Prelude.<*> (x Core..?> "DeviceTypeWindows")
            Prelude.<*> (x Core..?> "DeviceTypeIos")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ImportClientBranding where
  hashWithSalt _salt ImportClientBranding' {..} =
    _salt `Prelude.hashWithSalt` deviceTypeAndroid
      `Prelude.hashWithSalt` deviceTypeLinux
      `Prelude.hashWithSalt` deviceTypeWeb
      `Prelude.hashWithSalt` deviceTypeOsx
      `Prelude.hashWithSalt` deviceTypeWindows
      `Prelude.hashWithSalt` deviceTypeIos
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ImportClientBranding where
  rnf ImportClientBranding' {..} =
    Prelude.rnf deviceTypeAndroid
      `Prelude.seq` Prelude.rnf deviceTypeLinux
      `Prelude.seq` Prelude.rnf deviceTypeWeb
      `Prelude.seq` Prelude.rnf deviceTypeOsx
      `Prelude.seq` Prelude.rnf deviceTypeWindows
      `Prelude.seq` Prelude.rnf deviceTypeIos
      `Prelude.seq` Prelude.rnf resourceId

instance Core.ToHeaders ImportClientBranding where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "WorkspacesService.ImportClientBranding" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ImportClientBranding where
  toJSON ImportClientBranding' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeviceTypeAndroid" Core..=)
              Prelude.<$> deviceTypeAndroid,
            ("DeviceTypeLinux" Core..=)
              Prelude.<$> deviceTypeLinux,
            ("DeviceTypeWeb" Core..=) Prelude.<$> deviceTypeWeb,
            ("DeviceTypeOsx" Core..=) Prelude.<$> deviceTypeOsx,
            ("DeviceTypeWindows" Core..=)
              Prelude.<$> deviceTypeWindows,
            ("DeviceTypeIos" Core..=) Prelude.<$> deviceTypeIos,
            Prelude.Just ("ResourceId" Core..= resourceId)
          ]
      )

instance Core.ToPath ImportClientBranding where
  toPath = Prelude.const "/"

instance Core.ToQuery ImportClientBranding where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportClientBrandingResponse' smart constructor.
data ImportClientBrandingResponse = ImportClientBrandingResponse'
  { -- | The branding information configured for Android devices.
    deviceTypeAndroid :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information configured for Linux devices.
    deviceTypeLinux :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information configured for web access.
    deviceTypeWeb :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information configured for macOS devices.
    deviceTypeOsx :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information configured for Windows devices.
    deviceTypeWindows :: Prelude.Maybe DefaultClientBrandingAttributes,
    -- | The branding information configured for iOS devices.
    deviceTypeIos :: Prelude.Maybe IosClientBrandingAttributes,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportClientBrandingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceTypeAndroid', 'importClientBrandingResponse_deviceTypeAndroid' - The branding information configured for Android devices.
--
-- 'deviceTypeLinux', 'importClientBrandingResponse_deviceTypeLinux' - The branding information configured for Linux devices.
--
-- 'deviceTypeWeb', 'importClientBrandingResponse_deviceTypeWeb' - The branding information configured for web access.
--
-- 'deviceTypeOsx', 'importClientBrandingResponse_deviceTypeOsx' - The branding information configured for macOS devices.
--
-- 'deviceTypeWindows', 'importClientBrandingResponse_deviceTypeWindows' - The branding information configured for Windows devices.
--
-- 'deviceTypeIos', 'importClientBrandingResponse_deviceTypeIos' - The branding information configured for iOS devices.
--
-- 'httpStatus', 'importClientBrandingResponse_httpStatus' - The response's http status code.
newImportClientBrandingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportClientBrandingResponse
newImportClientBrandingResponse pHttpStatus_ =
  ImportClientBrandingResponse'
    { deviceTypeAndroid =
        Prelude.Nothing,
      deviceTypeLinux = Prelude.Nothing,
      deviceTypeWeb = Prelude.Nothing,
      deviceTypeOsx = Prelude.Nothing,
      deviceTypeWindows = Prelude.Nothing,
      deviceTypeIos = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The branding information configured for Android devices.
importClientBrandingResponse_deviceTypeAndroid :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
importClientBrandingResponse_deviceTypeAndroid = Lens.lens (\ImportClientBrandingResponse' {deviceTypeAndroid} -> deviceTypeAndroid) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeAndroid = a} :: ImportClientBrandingResponse)

-- | The branding information configured for Linux devices.
importClientBrandingResponse_deviceTypeLinux :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
importClientBrandingResponse_deviceTypeLinux = Lens.lens (\ImportClientBrandingResponse' {deviceTypeLinux} -> deviceTypeLinux) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeLinux = a} :: ImportClientBrandingResponse)

-- | The branding information configured for web access.
importClientBrandingResponse_deviceTypeWeb :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
importClientBrandingResponse_deviceTypeWeb = Lens.lens (\ImportClientBrandingResponse' {deviceTypeWeb} -> deviceTypeWeb) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeWeb = a} :: ImportClientBrandingResponse)

-- | The branding information configured for macOS devices.
importClientBrandingResponse_deviceTypeOsx :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
importClientBrandingResponse_deviceTypeOsx = Lens.lens (\ImportClientBrandingResponse' {deviceTypeOsx} -> deviceTypeOsx) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeOsx = a} :: ImportClientBrandingResponse)

-- | The branding information configured for Windows devices.
importClientBrandingResponse_deviceTypeWindows :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe DefaultClientBrandingAttributes)
importClientBrandingResponse_deviceTypeWindows = Lens.lens (\ImportClientBrandingResponse' {deviceTypeWindows} -> deviceTypeWindows) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeWindows = a} :: ImportClientBrandingResponse)

-- | The branding information configured for iOS devices.
importClientBrandingResponse_deviceTypeIos :: Lens.Lens' ImportClientBrandingResponse (Prelude.Maybe IosClientBrandingAttributes)
importClientBrandingResponse_deviceTypeIos = Lens.lens (\ImportClientBrandingResponse' {deviceTypeIos} -> deviceTypeIos) (\s@ImportClientBrandingResponse' {} a -> s {deviceTypeIos = a} :: ImportClientBrandingResponse)

-- | The response's http status code.
importClientBrandingResponse_httpStatus :: Lens.Lens' ImportClientBrandingResponse Prelude.Int
importClientBrandingResponse_httpStatus = Lens.lens (\ImportClientBrandingResponse' {httpStatus} -> httpStatus) (\s@ImportClientBrandingResponse' {} a -> s {httpStatus = a} :: ImportClientBrandingResponse)

instance Prelude.NFData ImportClientBrandingResponse where
  rnf ImportClientBrandingResponse' {..} =
    Prelude.rnf deviceTypeAndroid
      `Prelude.seq` Prelude.rnf deviceTypeLinux
      `Prelude.seq` Prelude.rnf deviceTypeWeb
      `Prelude.seq` Prelude.rnf deviceTypeOsx
      `Prelude.seq` Prelude.rnf deviceTypeWindows
      `Prelude.seq` Prelude.rnf deviceTypeIos
      `Prelude.seq` Prelude.rnf httpStatus
