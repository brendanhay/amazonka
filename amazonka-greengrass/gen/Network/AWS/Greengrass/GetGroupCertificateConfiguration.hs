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

import Network.AWS.Greengrass.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGroupCertificateConfiguration' smart constructor.
data GetGroupCertificateConfiguration = GetGroupCertificateConfiguration'
  { -- | The ID of the Greengrass group.
    groupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    GetGroupCertificateConfiguration
  where
  type
    Rs GetGroupCertificateConfiguration =
      GetGroupCertificateConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGroupCertificateConfigurationResponse'
            Prelude.<$> (x Prelude..?> "CertificateExpiryInMilliseconds")
            Prelude.<*> (x Prelude..?> "GroupId")
            Prelude.<*> ( x
                            Prelude..?> "CertificateAuthorityExpiryInMilliseconds"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetGroupCertificateConfiguration

instance
  Prelude.NFData
    GetGroupCertificateConfiguration

instance
  Prelude.ToHeaders
    GetGroupCertificateConfiguration
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
  Prelude.ToPath
    GetGroupCertificateConfiguration
  where
  toPath GetGroupCertificateConfiguration' {..} =
    Prelude.mconcat
      [ "/greengrass/groups/",
        Prelude.toBS groupId,
        "/certificateauthorities/configuration/expiry"
      ]

instance
  Prelude.ToQuery
    GetGroupCertificateConfiguration
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGroupCertificateConfigurationResponse' smart constructor.
data GetGroupCertificateConfigurationResponse = GetGroupCertificateConfigurationResponse'
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
  Prelude.Int ->
  GetGroupCertificateConfigurationResponse
newGetGroupCertificateConfigurationResponse
  pHttpStatus_ =
    GetGroupCertificateConfigurationResponse'
      { certificateExpiryInMilliseconds =
          Prelude.Nothing,
        groupId = Prelude.Nothing,
        certificateAuthorityExpiryInMilliseconds =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The amount of time remaining before the certificate expires, in
-- milliseconds.
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_certificateExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateExpiryInMilliseconds} -> certificateExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The ID of the group certificate configuration.
getGroupCertificateConfigurationResponse_groupId :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_groupId = Lens.lens (\GetGroupCertificateConfigurationResponse' {groupId} -> groupId) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {groupId = a} :: GetGroupCertificateConfigurationResponse)

-- | The amount of time remaining before the certificate authority expires,
-- in milliseconds.
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds :: Lens.Lens' GetGroupCertificateConfigurationResponse (Prelude.Maybe Prelude.Text)
getGroupCertificateConfigurationResponse_certificateAuthorityExpiryInMilliseconds = Lens.lens (\GetGroupCertificateConfigurationResponse' {certificateAuthorityExpiryInMilliseconds} -> certificateAuthorityExpiryInMilliseconds) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {certificateAuthorityExpiryInMilliseconds = a} :: GetGroupCertificateConfigurationResponse)

-- | The response's http status code.
getGroupCertificateConfigurationResponse_httpStatus :: Lens.Lens' GetGroupCertificateConfigurationResponse Prelude.Int
getGroupCertificateConfigurationResponse_httpStatus = Lens.lens (\GetGroupCertificateConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetGroupCertificateConfigurationResponse' {} a -> s {httpStatus = a} :: GetGroupCertificateConfigurationResponse)

instance
  Prelude.NFData
    GetGroupCertificateConfigurationResponse
