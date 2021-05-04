{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  UpdateGroupCertificateConfiguration
newUpdateGroupCertificateConfiguration pGroupId_ =
  UpdateGroupCertificateConfiguration'
    { certificateExpiryInMilliseconds =
        Prelude.Nothing,
      groupId = pGroupId_
    }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
updateGroupCertificateConfiguration_certificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfiguration (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfiguration_certificateExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfiguration' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@UpdateGroupCertificateConfiguration' {} a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfiguration)

-- | The ID of the Greengrass group.
updateGroupCertificateConfiguration_groupId :: Lens.Lens' UpdateGroupCertificateConfiguration Prelude.Text
updateGroupCertificateConfiguration_groupId = Lens.lens (\UpdateGroupCertificateConfiguration' {groupId} -> groupId) (\s@UpdateGroupCertificateConfiguration' {} a -> s {groupId = a} :: UpdateGroupCertificateConfiguration)

instance
  Prelude.AWSRequest
    UpdateGroupCertificateConfiguration
  where
  type
    Rs UpdateGroupCertificateConfiguration =
      UpdateGroupCertificateConfigurationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupCertificateConfigurationResponse'
            Prelude.<$> (x Prelude..?> "CertificateExpiryInMilliseconds")
              Prelude.<*> (x Prelude..?> "GroupId")
              Prelude.<*> ( x
                              Prelude..?> "CertificateAuthorityExpiryInMilliseconds"
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateGroupCertificateConfiguration

instance
  Prelude.NFData
    UpdateGroupCertificateConfiguration

instance
  Prelude.ToHeaders
    UpdateGroupCertificateConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateGroupCertificateConfiguration
  where
  toJSON UpdateGroupCertificateConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("CertificateExpiryInMilliseconds" Prelude..=)
              Prelude.<$> certificateExpiryInMilliseconds
          ]
      )

instance
  Prelude.ToPath
    UpdateGroupCertificateConfiguration
  where
  toPath UpdateGroupCertificateConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Prelude.ToQuery
    UpdateGroupCertificateConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
  { -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group certificate configuration.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The amount of time remaining before the certificate authority expires,
    -- in milliseconds.
    certificateAuthorityExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  UpdateGroupCertificateConfigurationResponse
newUpdateGroupCertificateConfigurationResponse
  pHttpStatus_ =
    UpdateGroupCertificateConfigurationResponse'
      { certificateExpiryInMilliseconds =
          Prelude.Nothing,
        groupId = Prelude.Nothing,
        certificateAuthorityExpiryInMilliseconds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
updateGroupCertificateConfigurationResponse_groupId :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_groupId = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The response's http status code.
updateGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' UpdateGroupCertificateConfigurationResponse Prelude.Int
updateGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateGroupCertificateConfigurationResponse)

instance
  Prelude.NFData
    UpdateGroupCertificateConfigurationResponse
