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
-- Module      : Amazonka.AlexaBusiness.PutInvitationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures the email template for the user enrollment invitation with
-- the specified attributes.
module Amazonka.AlexaBusiness.PutInvitationConfiguration
  ( -- * Creating a Request
    PutInvitationConfiguration (..),
    newPutInvitationConfiguration,

    -- * Request Lenses
    putInvitationConfiguration_contactEmail,
    putInvitationConfiguration_privateSkillIds,
    putInvitationConfiguration_organizationName,

    -- * Destructuring the Response
    PutInvitationConfigurationResponse (..),
    newPutInvitationConfigurationResponse,

    -- * Response Lenses
    putInvitationConfigurationResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutInvitationConfiguration' smart constructor.
data PutInvitationConfiguration = PutInvitationConfiguration'
  { -- | The email ID of the organization or individual contact that the enrolled
    -- user can use.
    contactEmail :: Prelude.Maybe Prelude.Text,
    -- | The list of private skill IDs that you want to recommend to the user to
    -- enable in the invitation.
    privateSkillIds :: Prelude.Maybe [Prelude.Text],
    -- | The name of the organization sending the enrollment invite to a user.
    organizationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInvitationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactEmail', 'putInvitationConfiguration_contactEmail' - The email ID of the organization or individual contact that the enrolled
-- user can use.
--
-- 'privateSkillIds', 'putInvitationConfiguration_privateSkillIds' - The list of private skill IDs that you want to recommend to the user to
-- enable in the invitation.
--
-- 'organizationName', 'putInvitationConfiguration_organizationName' - The name of the organization sending the enrollment invite to a user.
newPutInvitationConfiguration ::
  -- | 'organizationName'
  Prelude.Text ->
  PutInvitationConfiguration
newPutInvitationConfiguration pOrganizationName_ =
  PutInvitationConfiguration'
    { contactEmail =
        Prelude.Nothing,
      privateSkillIds = Prelude.Nothing,
      organizationName = pOrganizationName_
    }

-- | The email ID of the organization or individual contact that the enrolled
-- user can use.
putInvitationConfiguration_contactEmail :: Lens.Lens' PutInvitationConfiguration (Prelude.Maybe Prelude.Text)
putInvitationConfiguration_contactEmail = Lens.lens (\PutInvitationConfiguration' {contactEmail} -> contactEmail) (\s@PutInvitationConfiguration' {} a -> s {contactEmail = a} :: PutInvitationConfiguration)

-- | The list of private skill IDs that you want to recommend to the user to
-- enable in the invitation.
putInvitationConfiguration_privateSkillIds :: Lens.Lens' PutInvitationConfiguration (Prelude.Maybe [Prelude.Text])
putInvitationConfiguration_privateSkillIds = Lens.lens (\PutInvitationConfiguration' {privateSkillIds} -> privateSkillIds) (\s@PutInvitationConfiguration' {} a -> s {privateSkillIds = a} :: PutInvitationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The name of the organization sending the enrollment invite to a user.
putInvitationConfiguration_organizationName :: Lens.Lens' PutInvitationConfiguration Prelude.Text
putInvitationConfiguration_organizationName = Lens.lens (\PutInvitationConfiguration' {organizationName} -> organizationName) (\s@PutInvitationConfiguration' {} a -> s {organizationName = a} :: PutInvitationConfiguration)

instance Core.AWSRequest PutInvitationConfiguration where
  type
    AWSResponse PutInvitationConfiguration =
      PutInvitationConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutInvitationConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutInvitationConfiguration where
  hashWithSalt _salt PutInvitationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` contactEmail
      `Prelude.hashWithSalt` privateSkillIds
      `Prelude.hashWithSalt` organizationName

instance Prelude.NFData PutInvitationConfiguration where
  rnf PutInvitationConfiguration' {..} =
    Prelude.rnf contactEmail `Prelude.seq`
      Prelude.rnf privateSkillIds `Prelude.seq`
        Prelude.rnf organizationName

instance Data.ToHeaders PutInvitationConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.PutInvitationConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutInvitationConfiguration where
  toJSON PutInvitationConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ContactEmail" Data..=) Prelude.<$> contactEmail,
            ("PrivateSkillIds" Data..=)
              Prelude.<$> privateSkillIds,
            Prelude.Just
              ("OrganizationName" Data..= organizationName)
          ]
      )

instance Data.ToPath PutInvitationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutInvitationConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutInvitationConfigurationResponse' smart constructor.
data PutInvitationConfigurationResponse = PutInvitationConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutInvitationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putInvitationConfigurationResponse_httpStatus' - The response's http status code.
newPutInvitationConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutInvitationConfigurationResponse
newPutInvitationConfigurationResponse pHttpStatus_ =
  PutInvitationConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
putInvitationConfigurationResponse_httpStatus :: Lens.Lens' PutInvitationConfigurationResponse Prelude.Int
putInvitationConfigurationResponse_httpStatus = Lens.lens (\PutInvitationConfigurationResponse' {httpStatus} -> httpStatus) (\s@PutInvitationConfigurationResponse' {} a -> s {httpStatus = a} :: PutInvitationConfigurationResponse)

instance
  Prelude.NFData
    PutInvitationConfigurationResponse
  where
  rnf PutInvitationConfigurationResponse' {..} =
    Prelude.rnf httpStatus
