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
-- Module      : Amazonka.CustomerProfiles.DeleteProfileObjectType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a ProfileObjectType from a specific domain as well as removes
-- all the ProfileObjects of that type. It also disables integrations from
-- this specific ProfileObjectType. In addition, it scrubs all of the
-- fields of the standard profile that were populated from this
-- ProfileObjectType.
module Amazonka.CustomerProfiles.DeleteProfileObjectType
  ( -- * Creating a Request
    DeleteProfileObjectType (..),
    newDeleteProfileObjectType,

    -- * Request Lenses
    deleteProfileObjectType_domainName,
    deleteProfileObjectType_objectTypeName,

    -- * Destructuring the Response
    DeleteProfileObjectTypeResponse (..),
    newDeleteProfileObjectTypeResponse,

    -- * Response Lenses
    deleteProfileObjectTypeResponse_httpStatus,
    deleteProfileObjectTypeResponse_message,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteProfileObjectType' smart constructor.
data DeleteProfileObjectType = DeleteProfileObjectType'
  { -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileObjectType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteProfileObjectType_domainName' - The unique name of the domain.
--
-- 'objectTypeName', 'deleteProfileObjectType_objectTypeName' - The name of the profile object type.
newDeleteProfileObjectType ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  DeleteProfileObjectType
newDeleteProfileObjectType
  pDomainName_
  pObjectTypeName_ =
    DeleteProfileObjectType'
      { domainName = pDomainName_,
        objectTypeName = pObjectTypeName_
      }

-- | The unique name of the domain.
deleteProfileObjectType_domainName :: Lens.Lens' DeleteProfileObjectType Prelude.Text
deleteProfileObjectType_domainName = Lens.lens (\DeleteProfileObjectType' {domainName} -> domainName) (\s@DeleteProfileObjectType' {} a -> s {domainName = a} :: DeleteProfileObjectType)

-- | The name of the profile object type.
deleteProfileObjectType_objectTypeName :: Lens.Lens' DeleteProfileObjectType Prelude.Text
deleteProfileObjectType_objectTypeName = Lens.lens (\DeleteProfileObjectType' {objectTypeName} -> objectTypeName) (\s@DeleteProfileObjectType' {} a -> s {objectTypeName = a} :: DeleteProfileObjectType)

instance Core.AWSRequest DeleteProfileObjectType where
  type
    AWSResponse DeleteProfileObjectType =
      DeleteProfileObjectTypeResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteProfileObjectTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Message")
      )

instance Prelude.Hashable DeleteProfileObjectType where
  hashWithSalt _salt DeleteProfileObjectType' {..} =
    _salt
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` objectTypeName

instance Prelude.NFData DeleteProfileObjectType where
  rnf DeleteProfileObjectType' {..} =
    Prelude.rnf domainName `Prelude.seq`
      Prelude.rnf objectTypeName

instance Data.ToHeaders DeleteProfileObjectType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteProfileObjectType where
  toPath DeleteProfileObjectType' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/object-types/",
        Data.toBS objectTypeName
      ]

instance Data.ToQuery DeleteProfileObjectType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteProfileObjectTypeResponse' smart constructor.
data DeleteProfileObjectTypeResponse = DeleteProfileObjectTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A message that indicates the delete request is done.
    message :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteProfileObjectTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteProfileObjectTypeResponse_httpStatus' - The response's http status code.
--
-- 'message', 'deleteProfileObjectTypeResponse_message' - A message that indicates the delete request is done.
newDeleteProfileObjectTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'message'
  Prelude.Text ->
  DeleteProfileObjectTypeResponse
newDeleteProfileObjectTypeResponse
  pHttpStatus_
  pMessage_ =
    DeleteProfileObjectTypeResponse'
      { httpStatus =
          pHttpStatus_,
        message = pMessage_
      }

-- | The response's http status code.
deleteProfileObjectTypeResponse_httpStatus :: Lens.Lens' DeleteProfileObjectTypeResponse Prelude.Int
deleteProfileObjectTypeResponse_httpStatus = Lens.lens (\DeleteProfileObjectTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteProfileObjectTypeResponse' {} a -> s {httpStatus = a} :: DeleteProfileObjectTypeResponse)

-- | A message that indicates the delete request is done.
deleteProfileObjectTypeResponse_message :: Lens.Lens' DeleteProfileObjectTypeResponse Prelude.Text
deleteProfileObjectTypeResponse_message = Lens.lens (\DeleteProfileObjectTypeResponse' {message} -> message) (\s@DeleteProfileObjectTypeResponse' {} a -> s {message = a} :: DeleteProfileObjectTypeResponse)

instance
  Prelude.NFData
    DeleteProfileObjectTypeResponse
  where
  rnf DeleteProfileObjectTypeResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf message
