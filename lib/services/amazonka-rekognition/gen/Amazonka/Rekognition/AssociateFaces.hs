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
-- Module      : Amazonka.Rekognition.AssociateFaces
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates one or more faces with an existing UserID. Takes an array of
-- @FaceIds@. Each @FaceId@ that are present in the @FaceIds@ list is
-- associated with the provided UserID. The maximum number of total
-- @FaceIds@ per UserID is 100.
--
-- The @UserMatchThreshold@ parameter specifies the minimum user match
-- confidence required for the face to be associated with a UserID that has
-- at least one @FaceID@ already associated. This ensures that the
-- @FaceIds@ are associated with the right UserID. The value ranges from
-- 0-100 and default value is 75.
--
-- If successful, an array of @AssociatedFace@ objects containing the
-- associated @FaceIds@ is returned. If a given face is already associated
-- with the given @UserID@, it will be ignored and will not be returned in
-- the response. If a given face is already associated to a different
-- @UserID@, isn\'t found in the collection, doesn’t meet the
-- @UserMatchThreshold@, or there are already 100 faces associated with the
-- @UserID@, it will be returned as part of an array of
-- @UnsuccessfulFaceAssociations.@
--
-- The @UserStatus@ reflects the status of an operation which updates a
-- UserID representation with a list of given faces. The @UserStatus@ can
-- be:
--
-- -   ACTIVE - All associations or disassociations of FaceID(s) for a
--     UserID are complete.
--
-- -   CREATED - A UserID has been created, but has no FaceID(s) associated
--     with it.
--
-- -   UPDATING - A UserID is being updated and there are current
--     associations or disassociations of FaceID(s) taking place.
module Amazonka.Rekognition.AssociateFaces
  ( -- * Creating a Request
    AssociateFaces (..),
    newAssociateFaces,

    -- * Request Lenses
    associateFaces_clientRequestToken,
    associateFaces_userMatchThreshold,
    associateFaces_collectionId,
    associateFaces_userId,
    associateFaces_faceIds,

    -- * Destructuring the Response
    AssociateFacesResponse (..),
    newAssociateFacesResponse,

    -- * Response Lenses
    associateFacesResponse_associatedFaces,
    associateFacesResponse_unsuccessfulFaceAssociations,
    associateFacesResponse_userStatus,
    associateFacesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateFaces' smart constructor.
data AssociateFaces = AssociateFaces'
  { -- | Idempotent token used to identify the request to @AssociateFaces@. If
    -- you use the same token with multiple @AssociateFaces@ requests, the same
    -- response is returned. Use ClientRequestToken to prevent the same request
    -- from being processed more than once.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | An optional value specifying the minimum confidence in the UserID match
    -- to return. The default value is 75.
    userMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | The ID of an existing collection containing the UserID.
    collectionId :: Prelude.Text,
    -- | The ID for the existing UserID.
    userId :: Prelude.Text,
    -- | An array of FaceIDs to associate with the UserID.
    faceIds :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFaces' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRequestToken', 'associateFaces_clientRequestToken' - Idempotent token used to identify the request to @AssociateFaces@. If
-- you use the same token with multiple @AssociateFaces@ requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
--
-- 'userMatchThreshold', 'associateFaces_userMatchThreshold' - An optional value specifying the minimum confidence in the UserID match
-- to return. The default value is 75.
--
-- 'collectionId', 'associateFaces_collectionId' - The ID of an existing collection containing the UserID.
--
-- 'userId', 'associateFaces_userId' - The ID for the existing UserID.
--
-- 'faceIds', 'associateFaces_faceIds' - An array of FaceIDs to associate with the UserID.
newAssociateFaces ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'userId'
  Prelude.Text ->
  -- | 'faceIds'
  Prelude.NonEmpty Prelude.Text ->
  AssociateFaces
newAssociateFaces pCollectionId_ pUserId_ pFaceIds_ =
  AssociateFaces'
    { clientRequestToken =
        Prelude.Nothing,
      userMatchThreshold = Prelude.Nothing,
      collectionId = pCollectionId_,
      userId = pUserId_,
      faceIds = Lens.coerced Lens.# pFaceIds_
    }

-- | Idempotent token used to identify the request to @AssociateFaces@. If
-- you use the same token with multiple @AssociateFaces@ requests, the same
-- response is returned. Use ClientRequestToken to prevent the same request
-- from being processed more than once.
associateFaces_clientRequestToken :: Lens.Lens' AssociateFaces (Prelude.Maybe Prelude.Text)
associateFaces_clientRequestToken = Lens.lens (\AssociateFaces' {clientRequestToken} -> clientRequestToken) (\s@AssociateFaces' {} a -> s {clientRequestToken = a} :: AssociateFaces)

-- | An optional value specifying the minimum confidence in the UserID match
-- to return. The default value is 75.
associateFaces_userMatchThreshold :: Lens.Lens' AssociateFaces (Prelude.Maybe Prelude.Double)
associateFaces_userMatchThreshold = Lens.lens (\AssociateFaces' {userMatchThreshold} -> userMatchThreshold) (\s@AssociateFaces' {} a -> s {userMatchThreshold = a} :: AssociateFaces)

-- | The ID of an existing collection containing the UserID.
associateFaces_collectionId :: Lens.Lens' AssociateFaces Prelude.Text
associateFaces_collectionId = Lens.lens (\AssociateFaces' {collectionId} -> collectionId) (\s@AssociateFaces' {} a -> s {collectionId = a} :: AssociateFaces)

-- | The ID for the existing UserID.
associateFaces_userId :: Lens.Lens' AssociateFaces Prelude.Text
associateFaces_userId = Lens.lens (\AssociateFaces' {userId} -> userId) (\s@AssociateFaces' {} a -> s {userId = a} :: AssociateFaces)

-- | An array of FaceIDs to associate with the UserID.
associateFaces_faceIds :: Lens.Lens' AssociateFaces (Prelude.NonEmpty Prelude.Text)
associateFaces_faceIds = Lens.lens (\AssociateFaces' {faceIds} -> faceIds) (\s@AssociateFaces' {} a -> s {faceIds = a} :: AssociateFaces) Prelude.. Lens.coerced

instance Core.AWSRequest AssociateFaces where
  type
    AWSResponse AssociateFaces =
      AssociateFacesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFacesResponse'
            Prelude.<$> ( x
                            Data..?> "AssociatedFaces"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..?> "UnsuccessfulFaceAssociations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "UserStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFaces where
  hashWithSalt _salt AssociateFaces' {..} =
    _salt
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` userMatchThreshold
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` faceIds

instance Prelude.NFData AssociateFaces where
  rnf AssociateFaces' {..} =
    Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf userMatchThreshold
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf faceIds

instance Data.ToHeaders AssociateFaces where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.AssociateFaces" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFaces where
  toJSON AssociateFaces' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("UserMatchThreshold" Data..=)
              Prelude.<$> userMatchThreshold,
            Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("UserId" Data..= userId),
            Prelude.Just ("FaceIds" Data..= faceIds)
          ]
      )

instance Data.ToPath AssociateFaces where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFaces where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFacesResponse' smart constructor.
data AssociateFacesResponse = AssociateFacesResponse'
  { -- | An array of AssociatedFace objects containing FaceIDs that are
    -- successfully associated with the UserID is returned. Returned if the
    -- AssociateFaces action is successful.
    associatedFaces :: Prelude.Maybe [AssociatedFace],
    -- | An array of UnsuccessfulAssociation objects containing FaceIDs that are
    -- not successfully associated along with the reasons. Returned if the
    -- AssociateFaces action is successful.
    unsuccessfulFaceAssociations :: Prelude.Maybe [UnsuccessfulFaceAssociation],
    -- | The status of an update made to a UserID. Reflects if the UserID has
    -- been updated for every requested change.
    userStatus :: Prelude.Maybe UserStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFacesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedFaces', 'associateFacesResponse_associatedFaces' - An array of AssociatedFace objects containing FaceIDs that are
-- successfully associated with the UserID is returned. Returned if the
-- AssociateFaces action is successful.
--
-- 'unsuccessfulFaceAssociations', 'associateFacesResponse_unsuccessfulFaceAssociations' - An array of UnsuccessfulAssociation objects containing FaceIDs that are
-- not successfully associated along with the reasons. Returned if the
-- AssociateFaces action is successful.
--
-- 'userStatus', 'associateFacesResponse_userStatus' - The status of an update made to a UserID. Reflects if the UserID has
-- been updated for every requested change.
--
-- 'httpStatus', 'associateFacesResponse_httpStatus' - The response's http status code.
newAssociateFacesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFacesResponse
newAssociateFacesResponse pHttpStatus_ =
  AssociateFacesResponse'
    { associatedFaces =
        Prelude.Nothing,
      unsuccessfulFaceAssociations = Prelude.Nothing,
      userStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of AssociatedFace objects containing FaceIDs that are
-- successfully associated with the UserID is returned. Returned if the
-- AssociateFaces action is successful.
associateFacesResponse_associatedFaces :: Lens.Lens' AssociateFacesResponse (Prelude.Maybe [AssociatedFace])
associateFacesResponse_associatedFaces = Lens.lens (\AssociateFacesResponse' {associatedFaces} -> associatedFaces) (\s@AssociateFacesResponse' {} a -> s {associatedFaces = a} :: AssociateFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of UnsuccessfulAssociation objects containing FaceIDs that are
-- not successfully associated along with the reasons. Returned if the
-- AssociateFaces action is successful.
associateFacesResponse_unsuccessfulFaceAssociations :: Lens.Lens' AssociateFacesResponse (Prelude.Maybe [UnsuccessfulFaceAssociation])
associateFacesResponse_unsuccessfulFaceAssociations = Lens.lens (\AssociateFacesResponse' {unsuccessfulFaceAssociations} -> unsuccessfulFaceAssociations) (\s@AssociateFacesResponse' {} a -> s {unsuccessfulFaceAssociations = a} :: AssociateFacesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status of an update made to a UserID. Reflects if the UserID has
-- been updated for every requested change.
associateFacesResponse_userStatus :: Lens.Lens' AssociateFacesResponse (Prelude.Maybe UserStatus)
associateFacesResponse_userStatus = Lens.lens (\AssociateFacesResponse' {userStatus} -> userStatus) (\s@AssociateFacesResponse' {} a -> s {userStatus = a} :: AssociateFacesResponse)

-- | The response's http status code.
associateFacesResponse_httpStatus :: Lens.Lens' AssociateFacesResponse Prelude.Int
associateFacesResponse_httpStatus = Lens.lens (\AssociateFacesResponse' {httpStatus} -> httpStatus) (\s@AssociateFacesResponse' {} a -> s {httpStatus = a} :: AssociateFacesResponse)

instance Prelude.NFData AssociateFacesResponse where
  rnf AssociateFacesResponse' {..} =
    Prelude.rnf associatedFaces
      `Prelude.seq` Prelude.rnf unsuccessfulFaceAssociations
      `Prelude.seq` Prelude.rnf userStatus
      `Prelude.seq` Prelude.rnf httpStatus
