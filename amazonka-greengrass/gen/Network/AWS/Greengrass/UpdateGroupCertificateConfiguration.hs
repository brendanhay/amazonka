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
-- Module      : Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Certificate expiry time for a group.
module Network.AWS.Greengrass.UpdateGroupCertificateConfiguration
  ( -- * Creating a Request
    UpdateGroupCertificateConfiguration (..),
    newUpdateGroupCertificateConfiguration,

    -- * Request Lenses
    updateGroupCertificateConfiguration_certificateExpiryInMilliseconds,
    updateGroupCertificateConfiguration_groupId,

    -- * Destructuring the Response
    UpdateGroupCertificateConfigurationResponse (..),
    newUpdateGroupCertificateConfigurationResponse,

    -- * Response Lenses
    updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_groupId,
    updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Core.Maybe Core.Text,
    -- | The ID of the Greengrass group.
    groupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGroupCertificateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateExpiryInMilliseconds', 'updateGroupCertificateConfiguration_certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in
-- milliseconds.
--
-- 'groupId', 'updateGroupCertificateConfiguration_groupId' - The ID of the Greengrass group.
newUpdateGroupCertificateConfiguration ::
  -- | 'groupId'
  Core.Text ->
  UpdateGroupCertificateConfiguration
newUpdateGroupCertificateConfiguration pGroupId_ =
  UpdateGroupCertificateConfiguration'
    { certificateExpiryInMilliseconds =
        Core.Nothing,
      groupId = pGroupId_
    }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
updateGroupCertificateConfiguration_certificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfiguration (Core.Maybe Core.Text)
updateGroupCertificateConfiguration_certificateExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfiguration' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@UpdateGroupCertificateConfiguration' {} a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfiguration)

-- | The ID of the Greengrass group.
updateGroupCertificateConfiguration_groupId :: Lens.Lens' UpdateGroupCertificateConfiguration Core.Text
updateGroupCertificateConfiguration_groupId = Lens.lens (\UpdateGroupCertificateConfiguration' {groupId} -> groupId) (\s@UpdateGroupCertificateConfiguration' {} a -> s {groupId = a} :: UpdateGroupCertificateConfiguration)

instance
  Core.AWSRequest
    UpdateGroupCertificateConfiguration
  where
  type
    AWSResponse UpdateGroupCertificateConfiguration =
      UpdateGroupCertificateConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupCertificateConfigurationResponse'
            Core.<$> (x Core..?> "CertificateExpiryInMilliseconds")
            Core.<*> (x Core..?> "GroupId")
            Core.<*> ( x
                         Core..?> "CertificateAuthorityExpiryInMilliseconds"
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateGroupCertificateConfiguration

instance
  Core.NFData
    UpdateGroupCertificateConfiguration

instance
  Core.ToHeaders
    UpdateGroupCertificateConfiguration
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateGroupCertificateConfiguration
  where
  toJSON UpdateGroupCertificateConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CertificateExpiryInMilliseconds" Core..=)
              Core.<$> certificateExpiryInMilliseconds
          ]
      )

instance
  Core.ToPath
    UpdateGroupCertificateConfiguration
  where
  toPath UpdateGroupCertificateConfiguration' {..} =
    Core.mconcat
      [ "/greengrass/groups/",
        Core.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Core.ToQuery
    UpdateGroupCertificateConfiguration
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
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
-- Create a value of 'UpdateGroupCertificateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateExpiryInMilliseconds', 'updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in
-- milliseconds.
--
-- 'groupId', 'updateGroupCertificateConfigurationResponse_groupId' - The ID of the group certificate configuration.
--
-- 'certificateAuthorityExpiryInMilliseconds', 'updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires,
-- in milliseconds.
--
-- 'httpStatus', 'updateGroupCertificateConfigurationResponse_httpStatus' - The response's http status code.
newUpdateGroupCertificateConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGroupCertificateConfigurationResponse
newUpdateGroupCertificateConfigurationResponse
  pHttpStatus_ =
    UpdateGroupCertificateConfigurationResponse'
      { certificateExpiryInMilliseconds =
          Core.Nothing,
        groupId = Core.Nothing,
        certificateAuthorityExpiryInMilliseconds =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
updateGroupCertificateConfigurationResponse_groupId :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
updateGroupCertificateConfigurationResponse_groupId = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Core.Maybe Core.Text)
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The response's http status code.
updateGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' UpdateGroupCertificateConfigurationResponse Core.Int
updateGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateGroupCertificateConfigurationResponse)

instance
  Core.NFData
    UpdateGroupCertificateConfigurationResponse
