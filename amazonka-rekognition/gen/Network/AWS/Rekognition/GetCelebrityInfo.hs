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
-- Module      : Network.AWS.Rekognition.GetCelebrityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the name and additional information about a celebrity based on his
-- or her Amazon Rekognition ID. The additional information is returned as
-- an array of URLs. If there is no additional information about the
-- celebrity, this list is empty.
--
-- For more information, see Recognizing Celebrities in an Image in the
-- Amazon Rekognition Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:GetCelebrityInfo@ action.
module Network.AWS.Rekognition.GetCelebrityInfo
  ( -- * Creating a Request
    GetCelebrityInfo (..),
    newGetCelebrityInfo,

    -- * Request Lenses
    getCelebrityInfo_id,

    -- * Destructuring the Response
    GetCelebrityInfoResponse (..),
    newGetCelebrityInfoResponse,

    -- * Response Lenses
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCelebrityInfo' smart constructor.
data GetCelebrityInfo = GetCelebrityInfo'
  { -- | The ID for the celebrity. You get the celebrity ID from a call to the
    -- RecognizeCelebrities operation, which recognizes celebrities in an
    -- image.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCelebrityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCelebrityInfo_id' - The ID for the celebrity. You get the celebrity ID from a call to the
-- RecognizeCelebrities operation, which recognizes celebrities in an
-- image.
newGetCelebrityInfo ::
  -- | 'id'
  Core.Text ->
  GetCelebrityInfo
newGetCelebrityInfo pId_ =
  GetCelebrityInfo' {id = pId_}

-- | The ID for the celebrity. You get the celebrity ID from a call to the
-- RecognizeCelebrities operation, which recognizes celebrities in an
-- image.
getCelebrityInfo_id :: Lens.Lens' GetCelebrityInfo Core.Text
getCelebrityInfo_id = Lens.lens (\GetCelebrityInfo' {id} -> id) (\s@GetCelebrityInfo' {} a -> s {id = a} :: GetCelebrityInfo)

instance Core.AWSRequest GetCelebrityInfo where
  type
    AWSResponse GetCelebrityInfo =
      GetCelebrityInfoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCelebrityInfoResponse'
            Core.<$> (x Core..?> "Urls" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCelebrityInfo

instance Core.NFData GetCelebrityInfo

instance Core.ToHeaders GetCelebrityInfo where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.GetCelebrityInfo" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCelebrityInfo where
  toJSON GetCelebrityInfo' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Id" Core..= id)])

instance Core.ToPath GetCelebrityInfo where
  toPath = Core.const "/"

instance Core.ToQuery GetCelebrityInfo where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCelebrityInfoResponse' smart constructor.
data GetCelebrityInfoResponse = GetCelebrityInfoResponse'
  { -- | An array of URLs pointing to additional celebrity information.
    urls :: Core.Maybe [Core.Text],
    -- | The name of the celebrity.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCelebrityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'urls', 'getCelebrityInfoResponse_urls' - An array of URLs pointing to additional celebrity information.
--
-- 'name', 'getCelebrityInfoResponse_name' - The name of the celebrity.
--
-- 'httpStatus', 'getCelebrityInfoResponse_httpStatus' - The response's http status code.
newGetCelebrityInfoResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCelebrityInfoResponse
newGetCelebrityInfoResponse pHttpStatus_ =
  GetCelebrityInfoResponse'
    { urls = Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of URLs pointing to additional celebrity information.
getCelebrityInfoResponse_urls :: Lens.Lens' GetCelebrityInfoResponse (Core.Maybe [Core.Text])
getCelebrityInfoResponse_urls = Lens.lens (\GetCelebrityInfoResponse' {urls} -> urls) (\s@GetCelebrityInfoResponse' {} a -> s {urls = a} :: GetCelebrityInfoResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the celebrity.
getCelebrityInfoResponse_name :: Lens.Lens' GetCelebrityInfoResponse (Core.Maybe Core.Text)
getCelebrityInfoResponse_name = Lens.lens (\GetCelebrityInfoResponse' {name} -> name) (\s@GetCelebrityInfoResponse' {} a -> s {name = a} :: GetCelebrityInfoResponse)

-- | The response's http status code.
getCelebrityInfoResponse_httpStatus :: Lens.Lens' GetCelebrityInfoResponse Core.Int
getCelebrityInfoResponse_httpStatus = Lens.lens (\GetCelebrityInfoResponse' {httpStatus} -> httpStatus) (\s@GetCelebrityInfoResponse' {} a -> s {httpStatus = a} :: GetCelebrityInfoResponse)

instance Core.NFData GetCelebrityInfoResponse
