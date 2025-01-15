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
-- Module      : Amazonka.CustomerProfiles.DeleteProfileObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an object associated with a profile of a given
-- ProfileObjectType.
module Amazonka.CustomerProfiles.DeleteProfileObject
  ( -- * Creating a Request
    DeleteProfileObject (..),
    newDeleteProfileObject,

    -- * Request Lenses
    deleteProfileObject_profileId,
    deleteProfileObject_profileObjectUniqueKey,
    deleteProfileObject_objectTypeName,
    deleteProfileObject_domainName,

    -- * Destructuring the Response
    DeleteProfileObjectResponse (..),
    newDeleteProfileObjectResponse,

    -- * Response Lenses
    deleteProfileObjectResponse_message,
    deleteProfileObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProfileObject' smart constructor.
data DeleteProfileObject = DeleteProfileObject'
  { -- | The unique identifier of a customer profile.
    profileId :: Prelude.Text,
    -- | The unique identifier of the profile object generated by the service.
    profileObjectUniqueKey :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileId', 'deleteProfileObject_profileId' - The unique identifier of a customer profile.
--
-- 'profileObjectUniqueKey', 'deleteProfileObject_profileObjectUniqueKey' - The unique identifier of the profile object generated by the service.
--
-- 'objectTypeName', 'deleteProfileObject_objectTypeName' - The name of the profile object type.
--
-- 'domainName', 'deleteProfileObject_domainName' - The unique name of the domain.
newDeleteProfileObject ::
  -- | 'profileId'
  Prelude.Text ->
  -- | 'profileObjectUniqueKey'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  DeleteProfileObject
newDeleteProfileObject
  pProfileId_
  pProfileObjectUniqueKey_
  pObjectTypeName_
  pDomainName_ =
    DeleteProfileObject'
      { profileId = pProfileId_,
        profileObjectUniqueKey = pProfileObjectUniqueKey_,
        objectTypeName = pObjectTypeName_,
        domainName = pDomainName_
      }

-- | The unique identifier of a customer profile.
deleteProfileObject_profileId :: Lens.Lens' DeleteProfileObject Prelude.Text
deleteProfileObject_profileId = Lens.lens (\DeleteProfileObject' {profileId} -> profileId) (\s@DeleteProfileObject' {} a -> s {profileId = a} :: DeleteProfileObject)

-- | The unique identifier of the profile object generated by the service.
deleteProfileObject_profileObjectUniqueKey :: Lens.Lens' DeleteProfileObject Prelude.Text
deleteProfileObject_profileObjectUniqueKey = Lens.lens (\DeleteProfileObject' {profileObjectUniqueKey} -> profileObjectUniqueKey) (\s@DeleteProfileObject' {} a -> s {profileObjectUniqueKey = a} :: DeleteProfileObject)

-- | The name of the profile object type.
deleteProfileObject_objectTypeName :: Lens.Lens' DeleteProfileObject Prelude.Text
deleteProfileObject_objectTypeName = Lens.lens (\DeleteProfileObject' {objectTypeName} -> objectTypeName) (\s@DeleteProfileObject' {} a -> s {objectTypeName = a} :: DeleteProfileObject)

-- | The unique name of the domain.
deleteProfileObject_domainName :: Lens.Lens' DeleteProfileObject Prelude.Text
deleteProfileObject_domainName = Lens.lens (\DeleteProfileObject' {domainName} -> domainName) (\s@DeleteProfileObject' {} a -> s {domainName = a} :: DeleteProfileObject)

instance Core.AWSRequest DeleteProfileObject where
  type
    AWSResponse DeleteProfileObject =
      DeleteProfileObjectResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProfileObjectResponse'
            Prelude.<$> (x Data..?> "Message")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteProfileObject where
  hashWithSalt _salt DeleteProfileObject' {..} =
    _salt
      `Prelude.hashWithSalt` profileId
      `Prelude.hashWithSalt` profileObjectUniqueKey
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DeleteProfileObject where
  rnf DeleteProfileObject' {..} =
    Prelude.rnf profileId `Prelude.seq`
      Prelude.rnf profileObjectUniqueKey `Prelude.seq`
        Prelude.rnf objectTypeName `Prelude.seq`
          Prelude.rnf domainName

instance Data.ToHeaders DeleteProfileObject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteProfileObject where
  toJSON DeleteProfileObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ProfileId" Data..= profileId),
            Prelude.Just
              ( "ProfileObjectUniqueKey"
                  Data..= profileObjectUniqueKey
              ),
            Prelude.Just
              ("ObjectTypeName" Data..= objectTypeName)
          ]
      )

instance Data.ToPath DeleteProfileObject where
  toPath DeleteProfileObject' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/objects/delete"
      ]

instance Data.ToQuery DeleteProfileObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProfileObjectResponse' smart constructor.
data DeleteProfileObjectResponse = DeleteProfileObjectResponse'
  { -- | A message that indicates the delete request is done.
    message :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'deleteProfileObjectResponse_message' - A message that indicates the delete request is done.
--
-- 'httpStatus', 'deleteProfileObjectResponse_httpStatus' - The response's http status code.
newDeleteProfileObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteProfileObjectResponse
newDeleteProfileObjectResponse pHttpStatus_ =
  DeleteProfileObjectResponse'
    { message =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A message that indicates the delete request is done.
deleteProfileObjectResponse_message :: Lens.Lens' DeleteProfileObjectResponse (Prelude.Maybe Prelude.Text)
deleteProfileObjectResponse_message = Lens.lens (\DeleteProfileObjectResponse' {message} -> message) (\s@DeleteProfileObjectResponse' {} a -> s {message = a} :: DeleteProfileObjectResponse)

-- | The response's http status code.
deleteProfileObjectResponse_httpStatus :: Lens.Lens' DeleteProfileObjectResponse Prelude.Int
deleteProfileObjectResponse_httpStatus = Lens.lens (\DeleteProfileObjectResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileObjectResponse' {} a -> s {httpStatus = a} :: DeleteProfileObjectResponse)

instance Prelude.NFData DeleteProfileObjectResponse where
  rnf DeleteProfileObjectResponse' {..} =
    Prelude.rnf message `Prelude.seq`
      Prelude.rnf httpStatus
