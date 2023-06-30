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
-- Module      : Amazonka.CustomerProfiles.DeleteProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the standard customer profile and all data pertaining to the
-- profile.
module Amazonka.CustomerProfiles.DeleteProfile
  ( -- * Creating a Request
    DeleteProfile (..),
    newDeleteProfile,

    -- * Request Lenses
    deleteProfile_profileId,
    deleteProfile_domainName,

    -- * Destructuring the Response
    DeleteProfileResponse (..),
    newDeleteProfileResponse,

    -- * Response Lenses
    deleteProfileResponse_message,
    deleteProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProfile' smart constructor.
data DeleteProfile = DeleteProfile'
  { -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'deleteProfile_profileId' - The unique identifier of a customer profile.
--
-- 'domainName', 'deleteProfile_domainName' - The unique name of the domain.
newDeleteProfile ::
  -- | 'profileId'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeleteProfile
newDeleteProfile pProfileId_ pDomainName_ =
  DeleteProfile'
    { profileId = pProfileId_,
      domainName = pDomainName_
    }

-- | The unique identifier of a customer profile.
deleteProfile_profileId :: Lens.Lens' DeleteProfile Prelude.Text
deleteProfile_profileId = Lens.lens (\DeleteProfile' {profileId} -> profileId) (\s@DeleteProfile' {} a -> s {profileId = a} :: DeleteProfile)

-- | The unique name of the domain.
deleteProfile_domainName :: Lens.Lens' DeleteProfile Prelude.Text
deleteProfile_domainName = Lens.lens (\DeleteProfile' {domainName} -> domainName) (\s@DeleteProfile' {} a -> s {domainName = a} :: DeleteProfile)

instance Core.AWSRequest DeleteProfile where
  type
    AWSResponse DeleteProfile =
      DeleteProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProfileResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfile where
  hashWithSalt _salt DeleteProfile' {..} =
    _salt
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteProfile where
  rnf DeleteProfile' {..} =
    Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DeleteProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProfile where
  toJSON DeleteProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ProfileId" Data..= profileId)]
      )

instance Data.ToPath DeleteProfile where
  toPath DeleteProfile' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/delete"
      ]

instance Data.ToQuery DeleteProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProfileResponse' smart constructor.
data DeleteProfileResponse = DeleteProfileResponse'
  { -- | A message that indicates the delete request is done.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deleteProfileResponse_message' - A message that indicates the delete request is done.
--
-- 'httpStatus', 'deleteProfileResponse_httpStatus' - The response's http status code.
newDeleteProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProfileResponse
newDeleteProfileResponse pHttpStatus_ =
  DeleteProfileResponse'
    { message = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message that indicates the delete request is done.
deleteProfileResponse_message :: Lens.Lens' DeleteProfileResponse (Prelude.Maybe Prelude.Text)
deleteProfileResponse_message = Lens.lens (\DeleteProfileResponse' {message} -> message) (\s@DeleteProfileResponse' {} a -> s {message = a} :: DeleteProfileResponse)

-- | The response's http status code.
deleteProfileResponse_httpStatus :: Lens.Lens' DeleteProfileResponse Prelude.Int
deleteProfileResponse_httpStatus = Lens.lens (\DeleteProfileResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileResponse' {} a -> s {httpStatus = a} :: DeleteProfileResponse)

instance Prelude.NFData DeleteProfileResponse where
  rnf DeleteProfileResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
