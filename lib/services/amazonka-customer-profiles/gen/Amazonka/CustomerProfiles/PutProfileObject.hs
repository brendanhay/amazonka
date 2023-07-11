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
-- Module      : Amazonka.CustomerProfiles.PutProfileObject
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional objects to customer profiles of a given ObjectType.
--
-- When adding a specific profile object, like a Contact Record, an
-- inferred profile can get created if it is not mapped to an existing
-- profile. The resulting profile will only have a phone number populated
-- in the standard ProfileObject. Any additional Contact Records with the
-- same phone number will be mapped to the same inferred profile.
--
-- When a ProfileObject is created and if a ProfileObjectType already
-- exists for the ProfileObject, it will provide data to a standard profile
-- depending on the ProfileObjectType definition.
--
-- PutProfileObject needs an ObjectType, which can be created using
-- PutProfileObjectType.
module Amazonka.CustomerProfiles.PutProfileObject
  ( -- * Creating a Request
    PutProfileObject (..),
    newPutProfileObject,

    -- * Request Lenses
    putProfileObject_objectTypeName,
    putProfileObject_object,
    putProfileObject_domainName,

    -- * Destructuring the Response
    PutProfileObjectResponse (..),
    newPutProfileObjectResponse,

    -- * Response Lenses
    putProfileObjectResponse_profileObjectUniqueKey,
    putProfileObjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutProfileObject' smart constructor.
data PutProfileObject = PutProfileObject'
  { -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | A string that is serialized from a JSON object.
    object' :: Prelude.Text,
    -- | The unique name of the domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProfileObject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectTypeName', 'putProfileObject_objectTypeName' - The name of the profile object type.
--
-- 'object'', 'putProfileObject_object' - A string that is serialized from a JSON object.
--
-- 'domainName', 'putProfileObject_domainName' - The unique name of the domain.
newPutProfileObject ::
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'object''
  Prelude.Text ->
  -- | 'domainName'
  Prelude.Text ->
  PutProfileObject
newPutProfileObject
  pObjectTypeName_
  pObject_
  pDomainName_ =
    PutProfileObject'
      { objectTypeName =
          pObjectTypeName_,
        object' = pObject_,
        domainName = pDomainName_
      }

-- | The name of the profile object type.
putProfileObject_objectTypeName :: Lens.Lens' PutProfileObject Prelude.Text
putProfileObject_objectTypeName = Lens.lens (\PutProfileObject' {objectTypeName} -> objectTypeName) (\s@PutProfileObject' {} a -> s {objectTypeName = a} :: PutProfileObject)

-- | A string that is serialized from a JSON object.
putProfileObject_object :: Lens.Lens' PutProfileObject Prelude.Text
putProfileObject_object = Lens.lens (\PutProfileObject' {object'} -> object') (\s@PutProfileObject' {} a -> s {object' = a} :: PutProfileObject)

-- | The unique name of the domain.
putProfileObject_domainName :: Lens.Lens' PutProfileObject Prelude.Text
putProfileObject_domainName = Lens.lens (\PutProfileObject' {domainName} -> domainName) (\s@PutProfileObject' {} a -> s {domainName = a} :: PutProfileObject)

instance Core.AWSRequest PutProfileObject where
  type
    AWSResponse PutProfileObject =
      PutProfileObjectResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutProfileObjectResponse'
            Prelude.<$> (x Data..?> "ProfileObjectUniqueKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutProfileObject where
  hashWithSalt _salt PutProfileObject' {..} =
    _salt
      `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` object'
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData PutProfileObject where
  rnf PutProfileObject' {..} =
    Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf object'
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders PutProfileObject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutProfileObject where
  toJSON PutProfileObject' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ObjectTypeName" Data..= objectTypeName),
            Prelude.Just ("Object" Data..= object')
          ]
      )

instance Data.ToPath PutProfileObject where
  toPath PutProfileObject' {..} =
    Prelude.mconcat
      [ "/domains/",
        Data.toBS domainName,
        "/profiles/objects"
      ]

instance Data.ToQuery PutProfileObject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutProfileObjectResponse' smart constructor.
data PutProfileObjectResponse = PutProfileObjectResponse'
  { -- | The unique identifier of the profile object generated by the service.
    profileObjectUniqueKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutProfileObjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profileObjectUniqueKey', 'putProfileObjectResponse_profileObjectUniqueKey' - The unique identifier of the profile object generated by the service.
--
-- 'httpStatus', 'putProfileObjectResponse_httpStatus' - The response's http status code.
newPutProfileObjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutProfileObjectResponse
newPutProfileObjectResponse pHttpStatus_ =
  PutProfileObjectResponse'
    { profileObjectUniqueKey =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique identifier of the profile object generated by the service.
putProfileObjectResponse_profileObjectUniqueKey :: Lens.Lens' PutProfileObjectResponse (Prelude.Maybe Prelude.Text)
putProfileObjectResponse_profileObjectUniqueKey = Lens.lens (\PutProfileObjectResponse' {profileObjectUniqueKey} -> profileObjectUniqueKey) (\s@PutProfileObjectResponse' {} a -> s {profileObjectUniqueKey = a} :: PutProfileObjectResponse)

-- | The response's http status code.
putProfileObjectResponse_httpStatus :: Lens.Lens' PutProfileObjectResponse Prelude.Int
putProfileObjectResponse_httpStatus = Lens.lens (\PutProfileObjectResponse' {httpStatus} -> httpStatus) (\s@PutProfileObjectResponse' {} a -> s {httpStatus = a} :: PutProfileObjectResponse)

instance Prelude.NFData PutProfileObjectResponse where
  rnf PutProfileObjectResponse' {..} =
    Prelude.rnf profileObjectUniqueKey
      `Prelude.seq` Prelude.rnf httpStatus
