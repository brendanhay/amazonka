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
-- Module      : Amazonka.Rekognition.SearchUsersByImage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for UserIDs using a supplied image. It first detects the
-- largest face in the image, and then searches a specified collection for
-- matching UserIDs.
--
-- The operation returns an array of UserIDs that match the face in the
-- supplied image, ordered by similarity score with the highest similarity
-- first. It also returns a bounding box for the face found in the input
-- image.
--
-- Information about faces detected in the supplied image, but not used for
-- the search, is returned in an array of @UnsearchedFace@ objects. If no
-- valid face is detected in the image, the response will contain an empty
-- @UserMatches@ list and no @SearchedFace@ object.
module Amazonka.Rekognition.SearchUsersByImage
  ( -- * Creating a Request
    SearchUsersByImage (..),
    newSearchUsersByImage,

    -- * Request Lenses
    searchUsersByImage_maxUsers,
    searchUsersByImage_qualityFilter,
    searchUsersByImage_userMatchThreshold,
    searchUsersByImage_collectionId,
    searchUsersByImage_image,

    -- * Destructuring the Response
    SearchUsersByImageResponse (..),
    newSearchUsersByImageResponse,

    -- * Response Lenses
    searchUsersByImageResponse_faceModelVersion,
    searchUsersByImageResponse_searchedFace,
    searchUsersByImageResponse_unsearchedFaces,
    searchUsersByImageResponse_userMatches,
    searchUsersByImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchUsersByImage' smart constructor.
data SearchUsersByImage = SearchUsersByImage'
  { -- | Maximum number of UserIDs to return.
    maxUsers :: Prelude.Maybe Prelude.Natural,
    -- | A filter that specifies a quality bar for how much filtering is done to
    -- identify faces. Filtered faces aren\'t searched for in the collection.
    -- The default value is NONE.
    qualityFilter :: Prelude.Maybe QualityFilter,
    -- | Specifies the minimum confidence in the UserID match to return. Default
    -- value is 80.
    userMatchThreshold :: Prelude.Maybe Prelude.Double,
    -- | The ID of an existing collection containing the UserID.
    collectionId :: Prelude.Text,
    image :: Image
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchUsersByImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxUsers', 'searchUsersByImage_maxUsers' - Maximum number of UserIDs to return.
--
-- 'qualityFilter', 'searchUsersByImage_qualityFilter' - A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t searched for in the collection.
-- The default value is NONE.
--
-- 'userMatchThreshold', 'searchUsersByImage_userMatchThreshold' - Specifies the minimum confidence in the UserID match to return. Default
-- value is 80.
--
-- 'collectionId', 'searchUsersByImage_collectionId' - The ID of an existing collection containing the UserID.
--
-- 'image', 'searchUsersByImage_image' - Undocumented member.
newSearchUsersByImage ::
  -- | 'collectionId'
  Prelude.Text ->
  -- | 'image'
  Image ->
  SearchUsersByImage
newSearchUsersByImage pCollectionId_ pImage_ =
  SearchUsersByImage'
    { maxUsers = Prelude.Nothing,
      qualityFilter = Prelude.Nothing,
      userMatchThreshold = Prelude.Nothing,
      collectionId = pCollectionId_,
      image = pImage_
    }

-- | Maximum number of UserIDs to return.
searchUsersByImage_maxUsers :: Lens.Lens' SearchUsersByImage (Prelude.Maybe Prelude.Natural)
searchUsersByImage_maxUsers = Lens.lens (\SearchUsersByImage' {maxUsers} -> maxUsers) (\s@SearchUsersByImage' {} a -> s {maxUsers = a} :: SearchUsersByImage)

-- | A filter that specifies a quality bar for how much filtering is done to
-- identify faces. Filtered faces aren\'t searched for in the collection.
-- The default value is NONE.
searchUsersByImage_qualityFilter :: Lens.Lens' SearchUsersByImage (Prelude.Maybe QualityFilter)
searchUsersByImage_qualityFilter = Lens.lens (\SearchUsersByImage' {qualityFilter} -> qualityFilter) (\s@SearchUsersByImage' {} a -> s {qualityFilter = a} :: SearchUsersByImage)

-- | Specifies the minimum confidence in the UserID match to return. Default
-- value is 80.
searchUsersByImage_userMatchThreshold :: Lens.Lens' SearchUsersByImage (Prelude.Maybe Prelude.Double)
searchUsersByImage_userMatchThreshold = Lens.lens (\SearchUsersByImage' {userMatchThreshold} -> userMatchThreshold) (\s@SearchUsersByImage' {} a -> s {userMatchThreshold = a} :: SearchUsersByImage)

-- | The ID of an existing collection containing the UserID.
searchUsersByImage_collectionId :: Lens.Lens' SearchUsersByImage Prelude.Text
searchUsersByImage_collectionId = Lens.lens (\SearchUsersByImage' {collectionId} -> collectionId) (\s@SearchUsersByImage' {} a -> s {collectionId = a} :: SearchUsersByImage)

-- | Undocumented member.
searchUsersByImage_image :: Lens.Lens' SearchUsersByImage Image
searchUsersByImage_image = Lens.lens (\SearchUsersByImage' {image} -> image) (\s@SearchUsersByImage' {} a -> s {image = a} :: SearchUsersByImage)

instance Core.AWSRequest SearchUsersByImage where
  type
    AWSResponse SearchUsersByImage =
      SearchUsersByImageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchUsersByImageResponse'
            Prelude.<$> (x Data..?> "FaceModelVersion")
            Prelude.<*> (x Data..?> "SearchedFace")
            Prelude.<*> ( x
                            Data..?> "UnsearchedFaces"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "UserMatches" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchUsersByImage where
  hashWithSalt _salt SearchUsersByImage' {..} =
    _salt
      `Prelude.hashWithSalt` maxUsers
      `Prelude.hashWithSalt` qualityFilter
      `Prelude.hashWithSalt` userMatchThreshold
      `Prelude.hashWithSalt` collectionId
      `Prelude.hashWithSalt` image

instance Prelude.NFData SearchUsersByImage where
  rnf SearchUsersByImage' {..} =
    Prelude.rnf maxUsers
      `Prelude.seq` Prelude.rnf qualityFilter
      `Prelude.seq` Prelude.rnf userMatchThreshold
      `Prelude.seq` Prelude.rnf collectionId
      `Prelude.seq` Prelude.rnf image

instance Data.ToHeaders SearchUsersByImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RekognitionService.SearchUsersByImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SearchUsersByImage where
  toJSON SearchUsersByImage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxUsers" Data..=) Prelude.<$> maxUsers,
            ("QualityFilter" Data..=) Prelude.<$> qualityFilter,
            ("UserMatchThreshold" Data..=)
              Prelude.<$> userMatchThreshold,
            Prelude.Just ("CollectionId" Data..= collectionId),
            Prelude.Just ("Image" Data..= image)
          ]
      )

instance Data.ToPath SearchUsersByImage where
  toPath = Prelude.const "/"

instance Data.ToQuery SearchUsersByImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchUsersByImageResponse' smart constructor.
data SearchUsersByImageResponse = SearchUsersByImageResponse'
  { -- | Version number of the face detection model associated with the input
    -- collection CollectionId.
    faceModelVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of FaceDetail objects containing the BoundingBox for the largest
    -- face in image, as well as the confidence in the bounding box, that was
    -- searched for matches. If no valid face is detected in the image the
    -- response will contain no SearchedFace object.
    searchedFace :: Prelude.Maybe SearchedFaceDetails,
    -- | List of UnsearchedFace objects. Contains the face details infered from
    -- the specified image but not used for search. Contains reasons that
    -- describe why a face wasn\'t used for Search.
    unsearchedFaces :: Prelude.Maybe [UnsearchedFace],
    -- | An array of UserID objects that matched the input face, along with the
    -- confidence in the match. The returned structure will be empty if there
    -- are no matches. Returned if the SearchUsersByImageResponse action is
    -- successful.
    userMatches :: Prelude.Maybe [UserMatch],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchUsersByImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'faceModelVersion', 'searchUsersByImageResponse_faceModelVersion' - Version number of the face detection model associated with the input
-- collection CollectionId.
--
-- 'searchedFace', 'searchUsersByImageResponse_searchedFace' - A list of FaceDetail objects containing the BoundingBox for the largest
-- face in image, as well as the confidence in the bounding box, that was
-- searched for matches. If no valid face is detected in the image the
-- response will contain no SearchedFace object.
--
-- 'unsearchedFaces', 'searchUsersByImageResponse_unsearchedFaces' - List of UnsearchedFace objects. Contains the face details infered from
-- the specified image but not used for search. Contains reasons that
-- describe why a face wasn\'t used for Search.
--
-- 'userMatches', 'searchUsersByImageResponse_userMatches' - An array of UserID objects that matched the input face, along with the
-- confidence in the match. The returned structure will be empty if there
-- are no matches. Returned if the SearchUsersByImageResponse action is
-- successful.
--
-- 'httpStatus', 'searchUsersByImageResponse_httpStatus' - The response's http status code.
newSearchUsersByImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchUsersByImageResponse
newSearchUsersByImageResponse pHttpStatus_ =
  SearchUsersByImageResponse'
    { faceModelVersion =
        Prelude.Nothing,
      searchedFace = Prelude.Nothing,
      unsearchedFaces = Prelude.Nothing,
      userMatches = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Version number of the face detection model associated with the input
-- collection CollectionId.
searchUsersByImageResponse_faceModelVersion :: Lens.Lens' SearchUsersByImageResponse (Prelude.Maybe Prelude.Text)
searchUsersByImageResponse_faceModelVersion = Lens.lens (\SearchUsersByImageResponse' {faceModelVersion} -> faceModelVersion) (\s@SearchUsersByImageResponse' {} a -> s {faceModelVersion = a} :: SearchUsersByImageResponse)

-- | A list of FaceDetail objects containing the BoundingBox for the largest
-- face in image, as well as the confidence in the bounding box, that was
-- searched for matches. If no valid face is detected in the image the
-- response will contain no SearchedFace object.
searchUsersByImageResponse_searchedFace :: Lens.Lens' SearchUsersByImageResponse (Prelude.Maybe SearchedFaceDetails)
searchUsersByImageResponse_searchedFace = Lens.lens (\SearchUsersByImageResponse' {searchedFace} -> searchedFace) (\s@SearchUsersByImageResponse' {} a -> s {searchedFace = a} :: SearchUsersByImageResponse)

-- | List of UnsearchedFace objects. Contains the face details infered from
-- the specified image but not used for search. Contains reasons that
-- describe why a face wasn\'t used for Search.
searchUsersByImageResponse_unsearchedFaces :: Lens.Lens' SearchUsersByImageResponse (Prelude.Maybe [UnsearchedFace])
searchUsersByImageResponse_unsearchedFaces = Lens.lens (\SearchUsersByImageResponse' {unsearchedFaces} -> unsearchedFaces) (\s@SearchUsersByImageResponse' {} a -> s {unsearchedFaces = a} :: SearchUsersByImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | An array of UserID objects that matched the input face, along with the
-- confidence in the match. The returned structure will be empty if there
-- are no matches. Returned if the SearchUsersByImageResponse action is
-- successful.
searchUsersByImageResponse_userMatches :: Lens.Lens' SearchUsersByImageResponse (Prelude.Maybe [UserMatch])
searchUsersByImageResponse_userMatches = Lens.lens (\SearchUsersByImageResponse' {userMatches} -> userMatches) (\s@SearchUsersByImageResponse' {} a -> s {userMatches = a} :: SearchUsersByImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
searchUsersByImageResponse_httpStatus :: Lens.Lens' SearchUsersByImageResponse Prelude.Int
searchUsersByImageResponse_httpStatus = Lens.lens (\SearchUsersByImageResponse' {httpStatus} -> httpStatus) (\s@SearchUsersByImageResponse' {} a -> s {httpStatus = a} :: SearchUsersByImageResponse)

instance Prelude.NFData SearchUsersByImageResponse where
  rnf SearchUsersByImageResponse' {..} =
    Prelude.rnf faceModelVersion
      `Prelude.seq` Prelude.rnf searchedFace
      `Prelude.seq` Prelude.rnf unsearchedFaces
      `Prelude.seq` Prelude.rnf userMatches
      `Prelude.seq` Prelude.rnf httpStatus
