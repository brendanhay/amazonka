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
-- Module      : Network.AWS.Greengrass.GetGroupCertificateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current configuration for the CA used by the group.
module Network.AWS.Greengrass.GetGroupCertificateConfiguration
  ( -- * Creating a Request
    GetGroupCertificateConfiguration (..),
    newGetGroupCertificateConfiguration,

    -- * Request Lenses
    getGroupCertificateConfiguration_groupId,

    -- * Destructuring the Response
    GetGroupCertificateConfigurationResponse (..),
    newGetGroupCertificateConfigurationResponse,

    -- * Response Lenses
    getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_groupId,
    getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroupCertificateConfiguration' smart constructor.
data GetGroupCertificateConfiguration = GetGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupCertificateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'getGroupCertificateConfiguration_groupId' - The ID of the Greengrass group.
newGetGroupCertificateConfiguration ::
  -- | 'groupId'
  Core.Text ->
  GetGroupCertificateConfiguration
newGetGroupCertificateConfiguration pGroupId_ =
  GetGroupCertificateConfiguration'
    { groupId =
        pGroupId_
    }

-- | The ID of the Greengrass group.
getGroupCertificateConfiguration_groupId :: Lens.Lens' GetGroupCertificateConfiguration Core.Text
getGroupCertificateConfiguration_groupId = Lens.lens (\GetGroupCertificateConfiguration' {groupId} -> groupId) (\s@GetGroupCertificateConfiguration' {} a -> s {groupId = a} :: GetGroupCertificateConfiguration)

instance
  Core.AWSRequest
    GetGroupCertificateConfiguration
  where
  type
    AWSResponse GetGroupCertificateConfiguration =
      GetGroupCertificateConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupCertificateConfigurationResponse'
            Core.<$> (x Core..?> "CertificateExpiryInMilliseconds")
            Core.<*> (x Core..?> "GroupId")
            Core.<*> ( x
                         Core..?> "CertificateAuthorityExpiryInMilliseconds"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetGroupCertificateConfiguration

instance Core.NFData GetGroupCertificateConfiguration

instance
  Core.ToHeaders
    GetGroupCertificateConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath GetGroupCertificateConfiguration where
  toPath GetGroupCertificateConfiguration' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Core.ToQuery
    GetGroupCertificateConfiguration
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGroupCertificateConfigurationResponse' smart constructor.
data GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse'
  { -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Core.Maybe Core.Text,
    -- | The ID of the group certificate configuration.
    groupId :: Core.Maybe Core.Text,
    -- | The amount of time remaining before the certificate authority expires,
    -- in milliseconds.
    certificateAuthorityExpiryInMilliseconds :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGroupCertificateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateExpiryInMilliseconds', 'getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in
-- milliseconds.
--
-- 'groupId', 'getGroupCertificateConfigurationResponse_groupId' - The ID of the group certificate configuration.
--
-- 'certificateAuthorityExpiryInMilliseconds', 'getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires,
-- in milliseconds.
--
-- 'httpStatus', 'getGroupCertificateConfigurationResponse_httpStatus' - The response's http status code.
newGetGroupCertificateConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGroupCertificateConfigurationResponse
newGetGroupCertificateConfigurationResponse
  pHttpStatus_ =
    GetGroupCertificateConfigurationResponse'
      { certificateExpiryInMilliseconds =
          Core.Nothing,
        groupId = Core.Nothing,
        certificateAuthorityExpiryInMilliseconds =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
getGroupCertificateConfigurationResponse_groupId :: Lens.Lens' GetGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
getGroupCertificateConfigurationResponse_groupId = Lens.lens (\GetGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: GetGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The response's http status code.
getGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' GetGroupCertificateConfigurationResponse Core.Int
getGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\GetGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: GetGroupCertificateConfigurationResponse)

instance
  Core.NFData
    GetGroupCertificateConfigurationResponse
