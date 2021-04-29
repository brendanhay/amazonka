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
-- Module      : Network.AWS.AlexaBusiness.GetInvitationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the configured values for the user enrollment invitation email
-- template.
module Network.AWS.AlexaBusiness.GetInvitationConfiguration
  ( -- * Creating a Request
    GetInvitationConfiguration (..),
    newGetInvitationConfiguration,

    -- * Destructuring the Response
    GetInvitationConfigurationResponse (..),
    newGetInvitationConfigurationResponse,

    -- * Response Lenses
    getInvitationConfigurationResponse_organizationName,
    getInvitationConfigurationResponse_contactEmail,
    getInvitationConfigurationResponse_privateSkillIds,
    getInvitationConfigurationResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInvitationConfiguration' smart constructor.
data GetInvitationConfiguration = GetInvitationConfiguration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetInvitationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetInvitationConfiguration ::
  GetInvitationConfiguration
newGetInvitationConfiguration =
  GetInvitationConfiguration'

instance
  Prelude.AWSRequest
    GetInvitationConfiguration
  where
  type
    Rs GetInvitationConfiguration =
      GetInvitationConfigurationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInvitationConfigurationResponse'
            Prelude.<$> (x Prelude..?> "OrganizationName")
            Prelude.<*> (x Prelude..?> "ContactEmail")
            Prelude.<*> ( x Prelude..?> "PrivateSkillIds"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInvitationConfiguration

instance Prelude.NFData GetInvitationConfiguration

instance Prelude.ToHeaders GetInvitationConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "AlexaForBusiness.GetInvitationConfiguration" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetInvitationConfiguration where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetInvitationConfiguration where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetInvitationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInvitationConfigurationResponse' smart constructor.
data GetInvitationConfigurationResponse = GetInvitationConfigurationResponse'
  { -- | The name of the organization sending the enrollment invite to a user.
    organizationName :: Prelude.Maybe Prelude.Text,
    -- | The email ID of the organization or individual contact that the enrolled
    -- user can use.
    contactEmail :: Prelude.Maybe Prelude.Text,
    -- | The list of private skill IDs that you want to recommend to the user to
    -- enable in the invitation.
    privateSkillIds :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetInvitationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationName', 'getInvitationConfigurationResponse_organizationName' - The name of the organization sending the enrollment invite to a user.
--
-- 'contactEmail', 'getInvitationConfigurationResponse_contactEmail' - The email ID of the organization or individual contact that the enrolled
-- user can use.
--
-- 'privateSkillIds', 'getInvitationConfigurationResponse_privateSkillIds' - The list of private skill IDs that you want to recommend to the user to
-- enable in the invitation.
--
-- 'httpStatus', 'getInvitationConfigurationResponse_httpStatus' - The response's http status code.
newGetInvitationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetInvitationConfigurationResponse
newGetInvitationConfigurationResponse pHttpStatus_ =
  GetInvitationConfigurationResponse'
    { organizationName =
        Prelude.Nothing,
      contactEmail = Prelude.Nothing,
      privateSkillIds = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the organization sending the enrollment invite to a user.
getInvitationConfigurationResponse_organizationName :: Lens.Lens' GetInvitationConfigurationResponse (Prelude.Maybe Prelude.Text)
getInvitationConfigurationResponse_organizationName = Lens.lens (\GetInvitationConfigurationResponse' {organizationName} -> organizationName) (\s@GetInvitationConfigurationResponse' {} a -> s {organizationName = a} :: GetInvitationConfigurationResponse)

-- | The email ID of the organization or individual contact that the enrolled
-- user can use.
getInvitationConfigurationResponse_contactEmail :: Lens.Lens' GetInvitationConfigurationResponse (Prelude.Maybe Prelude.Text)
getInvitationConfigurationResponse_contactEmail = Lens.lens (\GetInvitationConfigurationResponse' {contactEmail} -> contactEmail) (\s@GetInvitationConfigurationResponse' {} a -> s {contactEmail = a} :: GetInvitationConfigurationResponse)

-- | The list of private skill IDs that you want to recommend to the user to
-- enable in the invitation.
getInvitationConfigurationResponse_privateSkillIds :: Lens.Lens' GetInvitationConfigurationResponse (Prelude.Maybe [Prelude.Text])
getInvitationConfigurationResponse_privateSkillIds = Lens.lens (\GetInvitationConfigurationResponse' {privateSkillIds} -> privateSkillIds) (\s@GetInvitationConfigurationResponse' {} a -> s {privateSkillIds = a} :: GetInvitationConfigurationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getInvitationConfigurationResponse_httpStatus :: Lens.Lens' GetInvitationConfigurationResponse Prelude.Int
getInvitationConfigurationResponse_httpStatus = Lens.lens (\GetInvitationConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetInvitationConfigurationResponse' {} a -> s {httpStatus = a} :: GetInvitationConfigurationResponse)

instance
  Prelude.NFData
    GetInvitationConfigurationResponse
