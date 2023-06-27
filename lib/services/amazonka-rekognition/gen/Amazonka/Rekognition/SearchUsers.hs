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
-- Module      : Amazonka.Rekognition.SearchUsers
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for UserIDs within a collection based on a @FaceId@ or
-- @UserId@. This API can be used to find the closest UserID (with a
-- highest similarity) to associate a face. The request must be provided
-- with either @FaceId@ or @UserId@. The operation returns an array of
-- UserID that match the @FaceId@ or @UserId@, ordered by similarity score
-- with the highest similarity first.
module Amazonka.Rekognition.SearchUsers
  ( -- * Creating a Request
    SearchUsers (..),
    newSearchUsers,

    -- * Request Lenses
    searchUsers_faceId,
    searchUsers_maxUsers,
    searchUsers_userId,
    searchUsers_userMatchThreshold,
    searchUsers_collectionId,

    -- * Destructuring the Response
    SearchUsersResponse (..),
    newSearchUsersResponse,

    -- * Response Lenses
    searchUsersResponse_faceModelVersion,
    searchUsersResponse_searchedFace,
    searchUsersResponse_searchedUser,
    searchUsersResponse_userMatches,
    searchUsersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchUsers' smart constructor.
data SearchUsers = SearchUsers'
  { -- | ID for the existing face.
    faceId :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of identities to return.
    maxUsers :: Prelude.Maybe Prelude.Natural,
    -- | ID for the existing User.
    userId :: Prelude.Maybe Prelude.Text,
    -- | Optional value that specifies the minimum confidence in the matched
    -- UserID to return. Default value of 80.
    userMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | The ID of an existing collection containing the UserID, used with a
    -- UserId or FaceId. If a FaceId is provided, UserId isn’t required to be
    -- present in the Collection.
    collectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchUsers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceId', 'searchUsers_faceId' - ID for the existing face.
--
-- 'maxUsers', 'searchUsers_maxUsers' - Maximum number of identities to return.
--
-- 'userId', 'searchUsers_userId' - ID for the existing User.
--
-- 'userMatchThreshold', 'searchUsers_userMatchThreshold' - Optional value that specifies the minimum confidence in the matched
-- UserID to return. Default value of 80.
--
-- 'collectionId', 'searchUsers_collectionId' - The ID of an existing collection containing the UserID, used with a
-- UserId or FaceId. If a FaceId is provided, UserId isn’t required to be
-- present in the Collection.
newSearchUsers ::
  -- | 'collectionId'
  Prelude.Text ->
  SearchUsers
newSearchUsers pCollectionId_ =
  SearchUsers'
    { faceId = Prelude.Nothing,
      maxUsers = Prelude.Nothing,
      userId = Prelude.Nothing,
      userMatchThreshold = Prelude.Nothing,
      collectionId = pCollectionId_
    }

-- | ID for the existing face.
searchUsers_faceId :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Text)
searchUsers_faceId = Lens.lens (\SearchUsers' {faceId} -> faceId) (\s@SearchUsers' {} a -> s {faceId = a} :: SearchUsers)

-- | Maximum number of identities to return.
searchUsers_maxUsers :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Natural)
searchUsers_maxUsers = Lens.lens (\SearchUsers' {maxUsers} -> maxUsers) (\s@SearchUsers' {} a -> s {maxUsers = a} :: SearchUsers)

-- | ID for the existing User.
searchUsers_userId :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Text)
searchUsers_userId = Lens.lens (\SearchUsers' {userId} -> userId) (\s@SearchUsers' {} a -> s {userId = a} :: SearchUsers)

-- | Optional value that specifies the minimum confidence in the matched
-- UserID to return. Default value of 80.
searchUsers_userMatchThreshold :: Lens.Lens' SearchUsers (Prelude.Maybe Prelude.Double)
searchUsers_userMatchThreshold = Lens.lens (\SearchUsers' {userMatchThreshold} -> userMatchThreshold) (\s@SearchUsers' {} a -> s {userMatchThreshold = a} :: SearchUsers)

-- | The ID of an existing collection containing the UserID, used with a
-- UserId or FaceId. If a FaceId is provided, UserId isn’t required to be
-- present in the Collection.
searchUsers_collectionId :: Lens.Lens' SearchUsers Prelude.Text
searchUsers_collectionId = Lens.lens (\SearchUsers' {collectionId} -> collectionId) (\s@SearchUsers' {} a -> s {collectionId = a} :: SearchUsers)

instance Core.AWSRequest SearchUsers where
  type AWSResponse SearchUsers = SearchUsersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersResponse'
            Prelude.<$> (x Data..?> "FaceModelVersion")
            Prelude.<*> (x Data..?> "SearchedFace")
            Prelude.<*> (x Data..?> "SearchedUser")
            Prelude.<*> (x Data..?> "UserMatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchUsers where
  hashWithSalt _salt SearchUsers' {..} =
    _salt
      `Prelude.hashWithSalt` faceId
      `Prelude.hashWithSalt` maxUsers
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userMatchThreshold
      `Prelude.hashWithSalt` collectionId

instance Prelude.NFData SearchUsers where
  rnf SearchUsers' {..} =
    Prelude.rnf faceId
      `Prelude.seq` Prelude.rnf maxUsers
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userMatchThreshold
      `Prelude.seq` Prelude.rnf collectionId

instance Data.ToHeaders SearchUsers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.SearchUsers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchUsers where
  toJSON SearchUsers' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FaceId" Data..=) Prelude.<$> faceId,
            ("MaxUsers" Data..=) Prelude.<$> maxUsers,
            ("UserId" Data..=) Prelude.<$> userId,
            ("UserMatchThreshold" Data..=)
              Prelude.<$> userMatchThreshold,
            Prelude.Just ("CollectionId" Data..= collectionId)
          ]
      )

instance Data.ToPath SearchUsers where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchUsers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchUsersResponse' smart constructor.
data SearchUsersResponse = SearchUsersResponse'
  { -- | Version number of the face detection model associated with the input
    -- CollectionId.
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | Contains the ID of a face that was used to search for matches in a
    -- collection.
    searchedFace :: Prelude.Maybe SearchedFace,
    -- | Contains the ID of the UserID that was used to search for matches in a
    -- collection.
    searchedUser :: Prelude.Maybe SearchedUser,
    -- | An array of UserMatch objects that matched the input face along with the
    -- confidence in the match. Array will be empty if there are no matches.
    userMatches :: Prelude.Maybe [UserMatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchUsersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'searchUsersResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- CollectionId.
--
-- 'searchedFace', 'searchUsersResponse_searchedFace' - Contains the ID of a face that was used to search for matches in a
-- collection.
--
-- 'searchedUser', 'searchUsersResponse_searchedUser' - Contains the ID of the UserID that was used to search for matches in a
-- collection.
--
-- 'userMatches', 'searchUsersResponse_userMatches' - An array of UserMatch objects that matched the input face along with the
-- confidence in the match. Array will be empty if there are no matches.
--
-- 'httpStatus', 'searchUsersResponse_httpStatus' - The response's http status code.
newSearchUsersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchUsersResponse
newSearchUsersResponse pHttpStatus_ =
  SearchUsersResponse'
    { faceModelVersion =
        Prelude.Nothing,
      searchedFace = Prelude.Nothing,
      searchedUser = Prelude.Nothing,
      userMatches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the input
-- CollectionId.
searchUsersResponse_faceModelVersion :: Lens.Lens' SearchUsersResponse (Prelude.Maybe Prelude.Text)
searchUsersResponse_faceModelVersion = Lens.lens (\SearchUsersResponse' {faceModelVersion} -> faceModelVersion) (\s@SearchUsersResponse' {} a -> s {faceModelVersion = a} :: SearchUsersResponse)

-- | Contains the ID of a face that was used to search for matches in a
-- collection.
searchUsersResponse_searchedFace :: Lens.Lens' SearchUsersResponse (Prelude.Maybe SearchedFace)
searchUsersResponse_searchedFace = Lens.lens (\SearchUsersResponse' {searchedFace} -> searchedFace) (\s@SearchUsersResponse' {} a -> s {searchedFace = a} :: SearchUsersResponse)

-- | Contains the ID of the UserID that was used to search for matches in a
-- collection.
searchUsersResponse_searchedUser :: Lens.Lens' SearchUsersResponse (Prelude.Maybe SearchedUser)
searchUsersResponse_searchedUser = Lens.lens (\SearchUsersResponse' {searchedUser} -> searchedUser) (\s@SearchUsersResponse' {} a -> s {searchedUser = a} :: SearchUsersResponse)

-- | An array of UserMatch objects that matched the input face along with the
-- confidence in the match. Array will be empty if there are no matches.
searchUsersResponse_userMatches :: Lens.Lens' SearchUsersResponse (Prelude.Maybe [UserMatch])
searchUsersResponse_userMatches = Lens.lens (\SearchUsersResponse' {userMatches} -> userMatches) (\s@SearchUsersResponse' {} a -> s {userMatches = a} :: SearchUsersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchUsersResponse_httpStatus :: Lens.Lens' SearchUsersResponse Prelude.Int
searchUsersResponse_httpStatus = Lens.lens (\SearchUsersResponse' {httpStatus} -> httpStatus) (\s@SearchUsersResponse' {} a -> s {httpStatus = a} :: SearchUsersResponse)

instance Prelude.NFData SearchUsersResponse where
  rnf SearchUsersResponse' {..} =
    Prelude.rnf faceModelVersion
      `Prelude.seq` Prelude.rnf searchedFace
      `Prelude.seq` Prelude.rnf searchedUser
      `Prelude.seq` Prelude.rnf userMatches
      `Prelude.seq` Prelude.rnf httpStatus
