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
-- Module      : Amazonka.Greengrass.GetGroupCertificateConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current configuration for the CA used by the group.
module Amazonka.Greengrass.GetGroupCertificateConfiguration
  ( -- * Creating a Request
    GetGroupCertificateConfiguration (..),
    newGetGroupCertificateConfiguration,

    -- * Request Lenses
    getGroupCertificateConfiguration_groupId,

    -- * Destructuring the Response
    GetGroupCertificateConfigurationResponse (..),
    newGetGroupCertificateConfigurationResponse,

    -- * Response Lenses
    getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds,
    getGroupCertificateConfigurationResponse_groupId,
    getGroupCertificateConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Greengrass.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGroupCertificateConfiguration' smart constructor.
data GetGroupCertificateConfiguration = GetGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetGroupCertificateConfiguration
newGetGroupCertificateConfiguration pGroupId_ =
  GetGroupCertificateConfiguration'
    { groupId =
        pGroupId_
    }

-- | The ID of the Greengrass group.
getGroupCertificateConfiguration_groupId :: Lens.Lens' GetGroupCertificateConfiguration Prelude.Text
getGroupCertificateConfiguration_groupId = Lens.lens (\GetGroupCertificateConfiguration' {groupId} -> groupId) (\s@GetGroupCertificateConfiguration' {} a -> s {groupId = a} :: GetGroupCertificateConfiguration)

instance
  Core.AWSRequest
    GetGroupCertificateConfiguration
  where
  type
    AWSResponse GetGroupCertificateConfiguration =
      GetGroupCertificateConfigurationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupCertificateConfigurationResponse'
            Prelude.<$> ( x
                            Data..?> "CertificateAuthorityExpiryInMilliseconds"
                        )
            Prelude.<*> (x Data..?> "CertificateExpiryInMilliseconds")
            Prelude.<*> (x Data..?> "GroupId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetGroupCertificateConfiguration
  where
  hashWithSalt
    _salt
    GetGroupCertificateConfiguration' {..} =
      _salt `Prelude.hashWithSalt` groupId

instance
  Prelude.NFData
    GetGroupCertificateConfiguration
  where
  rnf GetGroupCertificateConfiguration' {..} =
    Prelude.rnf groupId

instance
  Data.ToHeaders
    GetGroupCertificateConfiguration
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

instance Data.ToPath GetGroupCertificateConfiguration where
  toPath GetGroupCertificateConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Data.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Data.ToQuery
    GetGroupCertificateConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupCertificateConfigurationResponse' smart constructor.
data GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse'
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
-- Create a value of 'GetGroupCertificateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityExpiryInMilliseconds', 'getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds' - The amount of time remaining before the certificate authority expires,
-- in milliseconds.
--
-- 'certificateExpiryInMilliseconds', 'getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds' - The amount of time remaining before the certificate expires, in
-- milliseconds.
--
-- 'groupId', 'getGroupCertificateConfigurationResponse_groupId' - The ID of the group certificate configuration.
--
-- 'httpStatus', 'getGroupCertificateConfigurationResponse_httpStatus' - The response's http status code.
newGetGroupCertificateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGroupCertificateConfigurationResponse
newGetGroupCertificateConfigurationResponse
  pHttpStatus_ =
    GetGroupCertificateConfigurationResponse'
      { certificateAuthorityExpiryInMilliseconds =
          Prelude.Nothing,
        certificateExpiryInMilliseconds =
          Prelude.Nothing,
        groupId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
getGroupCertificateConfigurationResponse_groupId :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_groupId = Lens.lens (\GetGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: GetGroupCertificateConfigurationResponse)

-- | The response's http status code.
getGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' GetGroupCertificateConfigurationResponse Prelude.Int
getGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\GetGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: GetGroupCertificateConfigurationResponse)

instance
  Prelude.NFData
    GetGroupCertificateConfigurationResponse
  where
  rnf GetGroupCertificateConfigurationResponse' {..} =
    Prelude.rnf
      certificateAuthorityExpiryInMilliseconds
      `Prelude.seq` Prelude.rnf certificateExpiryInMilliseconds
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf httpStatus
