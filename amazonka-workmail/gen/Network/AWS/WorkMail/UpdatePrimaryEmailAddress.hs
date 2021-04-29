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
-- Module      : Network.AWS.WorkMail.UpdatePrimaryEmailAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the primary email for a user, group, or resource. The current
-- email is moved into the list of aliases (or swapped between an existing
-- alias and the current primary email), and the email provided in the
-- input is promoted as the primary.
module Network.AWS.WorkMail.UpdatePrimaryEmailAddress
  ( -- * Creating a Request
    UpdatePrimaryEmailAddress (..),
    newUpdatePrimaryEmailAddress,

    -- * Request Lenses
    updatePrimaryEmailAddress_organizationId,
    updatePrimaryEmailAddress_entityId,
    updatePrimaryEmailAddress_email,

    -- * Destructuring the Response
    UpdatePrimaryEmailAddressResponse (..),
    newUpdatePrimaryEmailAddressResponse,

    -- * Response Lenses
    updatePrimaryEmailAddressResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.WorkMail.Types

-- | /See:/ 'newUpdatePrimaryEmailAddress' smart constructor.
data UpdatePrimaryEmailAddress = UpdatePrimaryEmailAddress'
  { -- | The organization that contains the user, group, or resource to update.
    organizationId :: Prelude.Text,
    -- | The user, group, or resource to update.
    entityId :: Prelude.Text,
    -- | The value of the email to be updated as primary.
    email :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrimaryEmailAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationId', 'updatePrimaryEmailAddress_organizationId' - The organization that contains the user, group, or resource to update.
--
-- 'entityId', 'updatePrimaryEmailAddress_entityId' - The user, group, or resource to update.
--
-- 'email', 'updatePrimaryEmailAddress_email' - The value of the email to be updated as primary.
newUpdatePrimaryEmailAddress ::
  -- | 'organizationId'
  Prelude.Text ->
  -- | 'entityId'
  Prelude.Text ->
  -- | 'email'
  Prelude.Text ->
  UpdatePrimaryEmailAddress
newUpdatePrimaryEmailAddress
  pOrganizationId_
  pEntityId_
  pEmail_ =
    UpdatePrimaryEmailAddress'
      { organizationId =
          pOrganizationId_,
        entityId = pEntityId_,
        email = pEmail_
      }

-- | The organization that contains the user, group, or resource to update.
updatePrimaryEmailAddress_organizationId :: Lens.Lens' UpdatePrimaryEmailAddress Prelude.Text
updatePrimaryEmailAddress_organizationId = Lens.lens (\UpdatePrimaryEmailAddress' {organizationId} -> organizationId) (\s@UpdatePrimaryEmailAddress' {} a -> s {organizationId = a} :: UpdatePrimaryEmailAddress)

-- | The user, group, or resource to update.
updatePrimaryEmailAddress_entityId :: Lens.Lens' UpdatePrimaryEmailAddress Prelude.Text
updatePrimaryEmailAddress_entityId = Lens.lens (\UpdatePrimaryEmailAddress' {entityId} -> entityId) (\s@UpdatePrimaryEmailAddress' {} a -> s {entityId = a} :: UpdatePrimaryEmailAddress)

-- | The value of the email to be updated as primary.
updatePrimaryEmailAddress_email :: Lens.Lens' UpdatePrimaryEmailAddress Prelude.Text
updatePrimaryEmailAddress_email = Lens.lens (\UpdatePrimaryEmailAddress' {email} -> email) (\s@UpdatePrimaryEmailAddress' {} a -> s {email = a} :: UpdatePrimaryEmailAddress)

instance Prelude.AWSRequest UpdatePrimaryEmailAddress where
  type
    Rs UpdatePrimaryEmailAddress =
      UpdatePrimaryEmailAddressResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdatePrimaryEmailAddressResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePrimaryEmailAddress

instance Prelude.NFData UpdatePrimaryEmailAddress

instance Prelude.ToHeaders UpdatePrimaryEmailAddress where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "WorkMailService.UpdatePrimaryEmailAddress" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdatePrimaryEmailAddress where
  toJSON UpdatePrimaryEmailAddress' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("OrganizationId" Prelude..= organizationId),
            Prelude.Just ("EntityId" Prelude..= entityId),
            Prelude.Just ("Email" Prelude..= email)
          ]
      )

instance Prelude.ToPath UpdatePrimaryEmailAddress where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdatePrimaryEmailAddress where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePrimaryEmailAddressResponse' smart constructor.
data UpdatePrimaryEmailAddressResponse = UpdatePrimaryEmailAddressResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdatePrimaryEmailAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updatePrimaryEmailAddressResponse_httpStatus' - The response's http status code.
newUpdatePrimaryEmailAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePrimaryEmailAddressResponse
newUpdatePrimaryEmailAddressResponse pHttpStatus_ =
  UpdatePrimaryEmailAddressResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updatePrimaryEmailAddressResponse_httpStatus :: Lens.Lens' UpdatePrimaryEmailAddressResponse Prelude.Int
updatePrimaryEmailAddressResponse_httpStatus = Lens.lens (\UpdatePrimaryEmailAddressResponse' {httpStatus} -> httpStatus) (\s@UpdatePrimaryEmailAddressResponse' {} a -> s {httpStatus = a} :: UpdatePrimaryEmailAddressResponse)

instance
  Prelude.NFData
    UpdatePrimaryEmailAddressResponse
