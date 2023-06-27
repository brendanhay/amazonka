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
-- Module      : Amazonka.Greengrass.UpdateGroupCertificateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Certificate expiry time for a group.
module Amazonka.Greengrass.UpdateGroupCertificateConfiguration
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
    updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    updateGroupCertificateConfigurationResponse_groupId,
    updateGroupCertificateConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateGroupCertificateConfiguration' smart constructor.
data UpdateGroupCertificateConfiguration = UpdateGroupCertificateConfiguration'
  { -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    UpdateGroupCertificateConfiguration
  where
  type
    AWSResponse UpdateGroupCertificateConfiguration =
      UpdateGroupCertificateConfigurationResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateGroupCertificateConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "CertificateAuthorityExpiryInMilliseconds"
                        )
            Prelude.<*> (x Data..?> "CertificateExpiryInMilliseconds")
            Prelude.<*> (x Data..?> "GroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateGroupCertificateConfiguration
  where
  hashWithSalt
    _salt
    UpdateGroupCertificateConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` certificateExpiryInMilliseconds
        `Prelude.hashWithSalt` groupId

instance
  Prelude.NFData
    UpdateGroupCertificateConfiguration
  where
  rnf UpdateGroupCertificateConfiguration' {..} =
    Prelude.rnf certificateExpiryInMilliseconds
      `Prelude.seq` Prelude.rnf groupId

instance
  Data.ToHeaders
    UpdateGroupCertificateConfiguration
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateGroupCertificateConfiguration
  where
  toJSON UpdateGroupCertificateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateExpiryInMilliseconds" Data..=)
              Prelude.<$> certificateExpiryInMilliseconds
          ]
      )

instance
  Data.ToPath
    UpdateGroupCertificateConfiguration
  where
  toPath UpdateGroupCertificateConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Data.ToQuery
    UpdateGroupCertificateConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateGroupCertificateConfigurationResponse' smart constructor.
data UpdateGroupCertificateConfigurationResponse = UpdateGroupCertificateConfigurationResponse'
  { -- | The amount of time remaining before the certificate authority expires,
    -- in milliseconds.
    certificateAuthorityExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The amount of time remaining before the certificate expires, in
    -- milliseconds.
    certificateExpiryInMilliseconds :: Prelude.Maybe Prelude.Text,
    -- | The ID of the group certificate configuration.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroupCertificateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityExpiryInMilliseconds', 'updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires,
-- in milliseconds.
--
-- 'certificateExpiryInMilliseconds', 'updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in
-- milliseconds.
--
-- 'groupId', 'updateGroupCertificateConfigurationResponse_groupId' - The ID of the group certificate configuration.
--
-- 'httpStatus', 'updateGroupCertificateConfigurationResponse_httpStatus' - The response's http status code.
newUpdateGroupCertificateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateGroupCertificateConfigurationResponse
newUpdateGroupCertificateConfigurationResponse
  pHttpStatus_ =
    UpdateGroupCertificateConfigurationResponse'
      { certificateAuthorityExpiryInMilliseconds =
          Prelude.Nothing,
        certificateExpiryInMilliseconds =
          Prelude.Nothing,
        groupId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
updateGroupCertificateConfigurationResponse_groupId :: Lens.Lens' UpdateGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
updateGroupCertificateConfigurationResponse_groupId = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: UpdateGroupCertificateConfigurationResponse)

-- | The response's http status code.
updateGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' UpdateGroupCertificateConfigurationResponse Prelude.Int
updateGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\UpdateGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@UpdateGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: UpdateGroupCertificateConfigurationResponse)

instance
  Prelude.NFData
    UpdateGroupCertificateConfigurationResponse
  where
  rnf UpdateGroupCertificateConfigurationResponse' {..} =
    Prelude.rnf
      certificateAuthorityExpiryInMilliseconds
      `Prelude.seq` Prelude.rnf certificateExpiryInMilliseconds
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf httpStatus
