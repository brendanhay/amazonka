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
-- Module      : Amazonka.CustomerProfiles.DeleteProfileKey
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a searchable key from a customer profile.
module Amazonka.CustomerProfiles.DeleteProfileKey
  ( -- * Creating a Request
    DeleteProfileKey (..),
    newDeleteProfileKey,

    -- * Request Lenses
    deleteProfileKey_profileId,
    deleteProfileKey_keyName,
    deleteProfileKey_values,
    deleteProfileKey_domainName,

    -- * Destructuring the Response
    DeleteProfileKeyResponse (..),
    newDeleteProfileKeyResponse,

    -- * Response Lenses
    deleteProfileKeyResponse_message,
    deleteProfileKeyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProfileKey' smart constructor.
data DeleteProfileKey = DeleteProfileKey'
  { -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text,
    -- | A searchable identifier of a customer profile.
    keyName :: Prelude.Text,
    -- | A list of key values.
    values :: [Prelude.Text],
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileKey' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'deleteProfileKey_profileId' - The unique identifier of a customer profile.
--
-- 'keyName', 'deleteProfileKey_keyName' - A searchable identifier of a customer profile.
--
-- 'values', 'deleteProfileKey_values' - A list of key values.
--
-- 'domainName', 'deleteProfileKey_domainName' - The unique name of the domain.
newDeleteProfileKey ::
  -- | 'profileId'
  Prelude.Text ->
  -- | 'keyName'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeleteProfileKey
newDeleteProfileKey
  pProfileId_
  pKeyName_
  pDomainName_ =
    DeleteProfileKey'
      { profileId = pProfileId_,
        keyName = pKeyName_,
        values = Prelude.mempty,
        domainName = pDomainName_
      }

-- | The unique identifier of a customer profile.
deleteProfileKey_profileId :: Lens.Lens' DeleteProfileKey Prelude.Text
deleteProfileKey_profileId = Lens.lens (\DeleteProfileKey' {profileId} -> profileId) (\s@DeleteProfileKey' {} a -> s {profileId = a} :: DeleteProfileKey)

-- | A searchable identifier of a customer profile.
deleteProfileKey_keyName :: Lens.Lens' DeleteProfileKey Prelude.Text
deleteProfileKey_keyName = Lens.lens (\DeleteProfileKey' {keyName} -> keyName) (\s@DeleteProfileKey' {} a -> s {keyName = a} :: DeleteProfileKey)

-- | A list of key values.
deleteProfileKey_values :: Lens.Lens' DeleteProfileKey [Prelude.Text]
deleteProfileKey_values = Lens.lens (\DeleteProfileKey' {values} -> values) (\s@DeleteProfileKey' {} a -> s {values = a} :: DeleteProfileKey) Prelude.. Lens.coerced

-- | The unique name of the domain.
deleteProfileKey_domainName :: Lens.Lens' DeleteProfileKey Prelude.Text
deleteProfileKey_domainName = Lens.lens (\DeleteProfileKey' {domainName} -> domainName) (\s@DeleteProfileKey' {} a -> s {domainName = a} :: DeleteProfileKey)

instance Core.AWSRequest DeleteProfileKey where
  type
    AWSResponse DeleteProfileKey =
      DeleteProfileKeyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProfileKeyResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfileKey where
  hashWithSalt _salt DeleteProfileKey' {..} =
    _salt
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` keyName
      `Prelude.hashWithSalt` values
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteProfileKey where
  rnf DeleteProfileKey' {..} =
    Prelude.rnf profileId
      `Prelude.seq` Prelude.rnf keyName
      `Prelude.seq` Prelude.rnf values
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DeleteProfileKey where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProfileKey where
  toJSON DeleteProfileKey' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProfileId" Data..= profileId),
            Prelude.Just ("KeyName" Data..= keyName),
            Prelude.Just ("Values" Data..= values)
          ]
      )

instance Data.ToPath DeleteProfileKey where
  toPath DeleteProfileKey' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/keys/delete"
      ]

instance Data.ToQuery DeleteProfileKey where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProfileKeyResponse' smart constructor.
data DeleteProfileKeyResponse = DeleteProfileKeyResponse'
  { -- | A message that indicates the delete request is done.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileKeyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deleteProfileKeyResponse_message' - A message that indicates the delete request is done.
--
-- 'httpStatus', 'deleteProfileKeyResponse_httpStatus' - The response's http status code.
newDeleteProfileKeyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProfileKeyResponse
newDeleteProfileKeyResponse pHttpStatus_ =
  DeleteProfileKeyResponse'
    { message =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message that indicates the delete request is done.
deleteProfileKeyResponse_message :: Lens.Lens' DeleteProfileKeyResponse (Prelude.Maybe Prelude.Text)
deleteProfileKeyResponse_message = Lens.lens (\DeleteProfileKeyResponse' {message} -> message) (\s@DeleteProfileKeyResponse' {} a -> s {message = a} :: DeleteProfileKeyResponse)

-- | The response's http status code.
deleteProfileKeyResponse_httpStatus :: Lens.Lens' DeleteProfileKeyResponse Prelude.Int
deleteProfileKeyResponse_httpStatus = Lens.lens (\DeleteProfileKeyResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileKeyResponse' {} a -> s {httpStatus = a} :: DeleteProfileKeyResponse)

instance Prelude.NFData DeleteProfileKeyResponse where
  rnf DeleteProfileKeyResponse' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf httpStatus
