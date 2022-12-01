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
-- Module      : Amazonka.Rekognition.GetCelebrityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the name and additional information about a celebrity based on
-- their Amazon Rekognition ID. The additional information is returned as
-- an array of URLs. If there is no additional information about the
-- celebrity, this list is empty.
--
-- For more information, see Getting information about a celebrity in the
-- Amazon Rekognition Developer Guide.
--
-- This operation requires permissions to perform the
-- @rekognition:GetCelebrityInfo@ action.
module Amazonka.Rekognition.GetCelebrityInfo
  ( -- * Creating a Request
    GetCelebrityInfo (..),
    newGetCelebrityInfo,

    -- * Request Lenses
    getCelebrityInfo_id,

    -- * Destructuring the Response
    GetCelebrityInfoResponse (..),
    newGetCelebrityInfoResponse,

    -- * Response Lenses
    getCelebrityInfoResponse_name,
    getCelebrityInfoResponse_knownGender,
    getCelebrityInfoResponse_urls,
    getCelebrityInfoResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCelebrityInfo' smart constructor.
data GetCelebrityInfo = GetCelebrityInfo'
  { -- | The ID for the celebrity. You get the celebrity ID from a call to the
    -- RecognizeCelebrities operation, which recognizes celebrities in an
    -- image.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest GetCelebrityInfo where
  type
    AWSResponse GetCelebrityInfo =
      GetCelebrityInfoResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCelebrityInfoResponse'
            Prelude.<$> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "KnownGender")
            Prelude.<*> (x Core..?> "Urls" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCelebrityInfo where
  hashWithSalt _salt GetCelebrityInfo' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetCelebrityInfo where
  rnf GetCelebrityInfo' {..} = Prelude.rnf id

instance Core.ToHeaders GetCelebrityInfo where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RekognitionService.GetCelebrityInfo" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCelebrityInfo where
  toJSON GetCelebrityInfo' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("Id" Core..= id)])

instance Core.ToPath GetCelebrityInfo where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCelebrityInfo where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCelebrityInfoResponse' smart constructor.
data GetCelebrityInfoResponse = GetCelebrityInfoResponse'
  { -- | The name of the celebrity.
    name :: Prelude.Maybe Prelude.Text,
    -- | Retrieves the known gender for the celebrity.
    knownGender :: Prelude.Maybe KnownGender,
    -- | An array of URLs pointing to additional celebrity information.
    urls :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCelebrityInfoResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getCelebrityInfoResponse_name' - The name of the celebrity.
--
-- 'knownGender', 'getCelebrityInfoResponse_knownGender' - Retrieves the known gender for the celebrity.
--
-- 'urls', 'getCelebrityInfoResponse_urls' - An array of URLs pointing to additional celebrity information.
--
-- 'httpStatus', 'getCelebrityInfoResponse_httpStatus' - The response's http status code.
newGetCelebrityInfoResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCelebrityInfoResponse
newGetCelebrityInfoResponse pHttpStatus_ =
  GetCelebrityInfoResponse'
    { name = Prelude.Nothing,
      knownGender = Prelude.Nothing,
      urls = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the celebrity.
getCelebrityInfoResponse_name :: Lens.Lens' GetCelebrityInfoResponse (Prelude.Maybe Prelude.Text)
getCelebrityInfoResponse_name = Lens.lens (\GetCelebrityInfoResponse' {name} -> name) (\s@GetCelebrityInfoResponse' {} a -> s {name = a} :: GetCelebrityInfoResponse)

-- | Retrieves the known gender for the celebrity.
getCelebrityInfoResponse_knownGender :: Lens.Lens' GetCelebrityInfoResponse (Prelude.Maybe KnownGender)
getCelebrityInfoResponse_knownGender = Lens.lens (\GetCelebrityInfoResponse' {knownGender} -> knownGender) (\s@GetCelebrityInfoResponse' {} a -> s {knownGender = a} :: GetCelebrityInfoResponse)

-- | An array of URLs pointing to additional celebrity information.
getCelebrityInfoResponse_urls :: Lens.Lens' GetCelebrityInfoResponse (Prelude.Maybe [Prelude.Text])
getCelebrityInfoResponse_urls = Lens.lens (\GetCelebrityInfoResponse' {urls} -> urls) (\s@GetCelebrityInfoResponse' {} a -> s {urls = a} :: GetCelebrityInfoResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCelebrityInfoResponse_httpStatus :: Lens.Lens' GetCelebrityInfoResponse Prelude.Int
getCelebrityInfoResponse_httpStatus = Lens.lens (\GetCelebrityInfoResponse' {httpStatus} -> httpStatus) (\s@GetCelebrityInfoResponse' {} a -> s {httpStatus = a} :: GetCelebrityInfoResponse)

instance Prelude.NFData GetCelebrityInfoResponse where
  rnf GetCelebrityInfoResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf knownGender
      `Prelude.seq` Prelude.rnf urls
      `Prelude.seq` Prelude.rnf httpStatus
