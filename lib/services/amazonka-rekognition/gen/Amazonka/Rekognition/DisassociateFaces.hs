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
-- Module      : Amazonka.Rekognition.DisassociateFaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the association between a @Face@ supplied in an array of
-- @FaceIds@ and the User. If the User is not present already, then a
-- @ResourceNotFound@ exception is thrown. If successful, an array of faces
-- that are disassociated from the User is returned. If a given face is
-- already disassociated from the given UserID, it will be ignored and not
-- be returned in the response. If a given face is already associated with
-- a different User or not found in the collection it will be returned as
-- part of @UnsuccessfulDisassociations@. You can remove 1 - 100 face IDs
-- from a user at one time.
module Amazonka.Rekognition.DisassociateFaces
  ( -- * Creating a Request
    DisassociateFaces (..),
    newDisassociateFaces,

    -- * Request Lenses
    disassociateFaces_clientRequestToken,
    disassociateFaces_collectionId,
    disassociateFaces_userId,
    disassociateFaces_faceIds,

    -- * Destructuring the Response
    DisassociateFacesResponse (..),
    newDisassociateFacesResponse,

    -- * Response Lenses
    disassociateFacesResponse_disassociatedFaces,
    disassociateFacesResponse_unsuccessfulFaceDisassociations,
    disassociateFacesResponse_userStatus,
    disassociateFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateFaces' smart constructor.
data DisassociateFaces = DisassociateFaces'
  { -- | Idempotent token used to identify the request to @DisassociateFaces@. If
    -- you use the same token with multiple @DisassociateFaces@ requests, the
    -- same response is returned. Use ClientRequestToken to prevent the same
    -- request from being processed more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of an existing collection containing the UserID.
    collectionId :: Prelude.Text,
    -- | ID for the existing UserID.
    userId :: Prelude.Text,
    -- | An array of face IDs to disassociate from the UserID.
    faceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'disassociateFaces_clientRequestToken' - Idempotent token used to identify the request to @DisassociateFaces@. If
-- you use the same token with multiple @DisassociateFaces@ requests, the
-- same response is returned. Use ClientRequestToken to prevent the same
-- request from being processed more than once.
--
-- 'collectionId', 'disassociateFaces_collectionId' - The ID of an existing collection containing the UserID.
--
-- 'userId', 'disassociateFaces_userId' - ID for the existing UserID.
--
-- 'faceIds', 'disassociateFaces_faceIds' - An array of face IDs to disassociate from the UserID.
newDisassociateFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'faceIds'
  Prelude.NonEmpty Prelude.Text ->
  DisassociateFaces
newDisassociateFaces
  pCollectionId_
  pUserId_
  pFaceIds_ =
    DisassociateFaces'
      { clientRequestToken =
          Prelude.Nothing,
        collectionId = pCollectionId_,
        userId = pUserId_,
        faceIds = Lens.coerced Lens.# pFaceIds_
      }

-- | Idempotent token used to identify the request to @DisassociateFaces@. If
-- you use the same token with multiple @DisassociateFaces@ requests, the
-- same response is returned. Use ClientRequestToken to prevent the same
-- request from being processed more than once.
disassociateFaces_clientRequestToken :: Lens.Lens' DisassociateFaces (Prelude.Maybe Prelude.Text)
disassociateFaces_clientRequestToken = Lens.lens (\DisassociateFaces' {clientRequestToken} -> clientRequestToken) (\s@DisassociateFaces' {} a -> s {clientRequestToken = a} :: DisassociateFaces)

-- | The ID of an existing collection containing the UserID.
disassociateFaces_collectionId :: Lens.Lens' DisassociateFaces Prelude.Text
disassociateFaces_collectionId = Lens.lens (\DisassociateFaces' {collectionId} -> collectionId) (\s@DisassociateFaces' {} a -> s {collectionId = a} :: DisassociateFaces)

-- | ID for the existing UserID.
disassociateFaces_userId :: Lens.Lens' DisassociateFaces Prelude.Text
disassociateFaces_userId = Lens.lens (\DisassociateFaces' {userId} -> userId) (\s@DisassociateFaces' {} a -> s {userId = a} :: DisassociateFaces)

-- | An array of face IDs to disassociate from the UserID.
disassociateFaces_faceIds :: Lens.Lens' DisassociateFaces (Prelude.NonEmpty Prelude.Text)
disassociateFaces_faceIds = Lens.lens (\DisassociateFaces' {faceIds} -> faceIds) (\s@DisassociateFaces' {} a -> s {faceIds = a} :: DisassociateFaces) Prelude.. Lens.coerced

instance Core.AWSRequest DisassociateFaces where
  type
    AWSResponse DisassociateFaces =
      DisassociateFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateFacesResponse'
            Prelude.<$> ( x
                            Data..?> "DisassociatedFaces"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "UnsuccessfulFaceDisassociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "UserStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DisassociateFaces where
  hashWithSalt _salt DisassociateFaces' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` faceIds

instance Prelude.NFData DisassociateFaces where
  rnf DisassociateFaces' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf faceIds

instance Data.ToHeaders DisassociateFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.DisassociateFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DisassociateFaces where
  toJSON DisassociateFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("UserId" Data..= userId),
            Prelude.Just ("FaceIds" Data..= faceIds)
          ]
      )

instance Data.ToPath DisassociateFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery DisassociateFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateFacesResponse' smart constructor.
data DisassociateFacesResponse = DisassociateFacesResponse'
  { -- | An array of DissociatedFace objects containing FaceIds that are
    -- successfully disassociated with the UserID is returned. Returned if the
    -- DisassociatedFaces action is successful.
    disassociatedFaces :: Prelude.Maybe [DisassociatedFace],
    -- | An array of UnsuccessfulDisassociation objects containing FaceIds that
    -- are not successfully associated, along with the reasons for the failure
    -- to associate. Returned if the DisassociateFaces action is successful.
    unsuccessfulFaceDisassociations :: Prelude.Maybe [UnsuccessfulFaceDisassociation],
    -- | The status of an update made to a User. Reflects if the User has been
    -- updated for every requested change.
    userStatus :: Prelude.Maybe UserStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disassociatedFaces', 'disassociateFacesResponse_disassociatedFaces' - An array of DissociatedFace objects containing FaceIds that are
-- successfully disassociated with the UserID is returned. Returned if the
-- DisassociatedFaces action is successful.
--
-- 'unsuccessfulFaceDisassociations', 'disassociateFacesResponse_unsuccessfulFaceDisassociations' - An array of UnsuccessfulDisassociation objects containing FaceIds that
-- are not successfully associated, along with the reasons for the failure
-- to associate. Returned if the DisassociateFaces action is successful.
--
-- 'userStatus', 'disassociateFacesResponse_userStatus' - The status of an update made to a User. Reflects if the User has been
-- updated for every requested change.
--
-- 'httpStatus', 'disassociateFacesResponse_httpStatus' - The response's http status code.
newDisassociateFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateFacesResponse
newDisassociateFacesResponse pHttpStatus_ =
  DisassociateFacesResponse'
    { disassociatedFaces =
        Prelude.Nothing,
      unsuccessfulFaceDisassociations =
        Prelude.Nothing,
      userStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of DissociatedFace objects containing FaceIds that are
-- successfully disassociated with the UserID is returned. Returned if the
-- DisassociatedFaces action is successful.
disassociateFacesResponse_disassociatedFaces :: Lens.Lens' DisassociateFacesResponse (Prelude.Maybe [DisassociatedFace])
disassociateFacesResponse_disassociatedFaces = Lens.lens (\DisassociateFacesResponse' {disassociatedFaces} -> disassociatedFaces) (\s@DisassociateFacesResponse' {} a -> s {disassociatedFaces = a} :: DisassociateFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of UnsuccessfulDisassociation objects containing FaceIds that
-- are not successfully associated, along with the reasons for the failure
-- to associate. Returned if the DisassociateFaces action is successful.
disassociateFacesResponse_unsuccessfulFaceDisassociations :: Lens.Lens' DisassociateFacesResponse (Prelude.Maybe [UnsuccessfulFaceDisassociation])
disassociateFacesResponse_unsuccessfulFaceDisassociations = Lens.lens (\DisassociateFacesResponse' {unsuccessfulFaceDisassociations} -> unsuccessfulFaceDisassociations) (\s@DisassociateFacesResponse' {} a -> s {unsuccessfulFaceDisassociations = a} :: DisassociateFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of an update made to a User. Reflects if the User has been
-- updated for every requested change.
disassociateFacesResponse_userStatus :: Lens.Lens' DisassociateFacesResponse (Prelude.Maybe UserStatus)
disassociateFacesResponse_userStatus = Lens.lens (\DisassociateFacesResponse' {userStatus} -> userStatus) (\s@DisassociateFacesResponse' {} a -> s {userStatus = a} :: DisassociateFacesResponse)

-- | The response's http status code.
disassociateFacesResponse_httpStatus :: Lens.Lens' DisassociateFacesResponse Prelude.Int
disassociateFacesResponse_httpStatus = Lens.lens (\DisassociateFacesResponse' {httpStatus} -> httpStatus) (\s@DisassociateFacesResponse' {} a -> s {httpStatus = a} :: DisassociateFacesResponse)

instance Prelude.NFData DisassociateFacesResponse where
  rnf DisassociateFacesResponse' {..} =
    Prelude.rnf disassociatedFaces
      `Prelude.seq` Prelude.rnf unsuccessfulFaceDisassociations
      `Prelude.seq` Prelude.rnf userStatus
      `Prelude.seq` Prelude.rnf httpStatus
