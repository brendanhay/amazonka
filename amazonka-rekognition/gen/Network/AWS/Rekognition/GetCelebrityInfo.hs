{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCelebrityInfo' smart constructor.
data GetCelebrityInfo = GetCelebrityInfo'
  { -- | The ID for the celebrity. You get the celebrity ID from a call to the
    -- RecognizeCelebrities operation, which recognizes celebrities in an
    -- image.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetCelebrityInfo
newGetCelebrityInfo pId_ =
  GetCelebrityInfo' {id = pId_}

-- | The ID for the celebrity. You get the celebrity ID from a call to the
-- RecognizeCelebrities operation, which recognizes celebrities in an
-- image.
getCelebrityInfo_id :: Lens.Lens' GetCelebrityInfo Prelude.Text
getCelebrityInfo_id = Lens.lens (\GetCelebrityInfo' {id} -> id) (\s@GetCelebrityInfo' {} a -> s {id = a} :: GetCelebrityInfo)

instance Prelude.AWSRequest GetCelebrityInfo where
  type Rs GetCelebrityInfo = GetCelebrityInfoResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCelebrityInfoResponse'
            Prelude.<$> (x Prelude..?> "Urls" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCelebrityInfo

instance Prelude.NFData GetCelebrityInfo

instance Prelude.ToHeaders GetCelebrityInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "RekognitionService.GetCelebrityInfo" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetCelebrityInfo where
  toJSON GetCelebrityInfo' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("Id" Prelude..= id)]
      )

instance Prelude.ToPath GetCelebrityInfo where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetCelebrityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCelebrityInfoResponse' smart constructor.
data GetCelebrityInfoResponse = GetCelebrityInfoResponse'
  { -- | An array of URLs pointing to additional celebrity information.
    urls :: Prelude.Maybe [Prelude.Text],
    -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetCelebrityInfoResponse
newGetCelebrityInfoResponse pHttpStatus_ =
  GetCelebrityInfoResponse'
    { urls = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of URLs pointing to additional celebrity information.
getCelebrityInfoResponse_urls :: Lens.Lens' GetCelebrityInfoResponse (Prelude.Maybe [Prelude.Text])
getCelebrityInfoResponse_urls = Lens.lens (\GetCelebrityInfoResponse' {urls} -> urls) (\s@GetCelebrityInfoResponse' {} a -> s {urls = a} :: GetCelebrityInfoResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the celebrity.
getCelebrityInfoResponse_name :: Lens.Lens' GetCelebrityInfoResponse (Prelude.Maybe Prelude.Text)
getCelebrityInfoResponse_name = Lens.lens (\GetCelebrityInfoResponse' {name} -> name) (\s@GetCelebrityInfoResponse' {} a -> s {name = a} :: GetCelebrityInfoResponse)

-- | The response's http status code.
getCelebrityInfoResponse_httpStatus :: Lens.Lens' GetCelebrityInfoResponse Prelude.Int
getCelebrityInfoResponse_httpStatus = Lens.lens (\GetCelebrityInfoResponse' {httpStatus} -> httpStatus) (\s@GetCelebrityInfoResponse' {} a -> s {httpStatus = a} :: GetCelebrityInfoResponse)

instance Prelude.NFData GetCelebrityInfoResponse
