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
-- Module      : Amazonka.IoTFleetWise.GetDecoderManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a created decoder manifest.
module Amazonka.IoTFleetWise.GetDecoderManifest
  ( -- * Creating a Request
    GetDecoderManifest (..),
    newGetDecoderManifest,

    -- * Request Lenses
    getDecoderManifest_name,

    -- * Destructuring the Response
    GetDecoderManifestResponse (..),
    newGetDecoderManifestResponse,

    -- * Response Lenses
    getDecoderManifestResponse_description,
    getDecoderManifestResponse_modelManifestArn,
    getDecoderManifestResponse_status,
    getDecoderManifestResponse_httpStatus,
    getDecoderManifestResponse_name,
    getDecoderManifestResponse_arn,
    getDecoderManifestResponse_creationTime,
    getDecoderManifestResponse_lastModificationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDecoderManifest' smart constructor.
data GetDecoderManifest = GetDecoderManifest'
  { -- | The name of the decoder manifest to retrieve information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDecoderManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getDecoderManifest_name' - The name of the decoder manifest to retrieve information about.
newGetDecoderManifest ::
  -- | 'name'
  Prelude.Text ->
  GetDecoderManifest
newGetDecoderManifest pName_ =
  GetDecoderManifest' {name = pName_}

-- | The name of the decoder manifest to retrieve information about.
getDecoderManifest_name :: Lens.Lens' GetDecoderManifest Prelude.Text
getDecoderManifest_name = Lens.lens (\GetDecoderManifest' {name} -> name) (\s@GetDecoderManifest' {} a -> s {name = a} :: GetDecoderManifest)

instance Core.AWSRequest GetDecoderManifest where
  type
    AWSResponse GetDecoderManifest =
      GetDecoderManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDecoderManifestResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "modelManifestArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "lastModificationTime")
      )

instance Prelude.Hashable GetDecoderManifest where
  hashWithSalt _salt GetDecoderManifest' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetDecoderManifest where
  rnf GetDecoderManifest' {..} = Prelude.rnf name

instance Data.ToHeaders GetDecoderManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetDecoderManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDecoderManifest where
  toJSON GetDecoderManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetDecoderManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDecoderManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDecoderManifestResponse' smart constructor.
data GetDecoderManifestResponse = GetDecoderManifestResponse'
  { -- | A brief description of the decoder manifest.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a vehicle model (model manifest) associated with the decoder
    -- manifest.
    modelManifestArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the decoder manifest. If the status is @ACTIVE@, the
    -- decoder manifest can\'t be edited. If the status is marked @DRAFT@, you
    -- can edit the decoder manifest.
    status :: Prelude.Maybe ManifestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the decoder manifest.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the decoder manifest.
    arn :: Prelude.Text,
    -- | The time the decoder manifest was created in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The time the decoder manifest was last updated in seconds since epoch
    -- (January 1, 1970 at midnight UTC time).
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDecoderManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getDecoderManifestResponse_description' - A brief description of the decoder manifest.
--
-- 'modelManifestArn', 'getDecoderManifestResponse_modelManifestArn' - The ARN of a vehicle model (model manifest) associated with the decoder
-- manifest.
--
-- 'status', 'getDecoderManifestResponse_status' - The state of the decoder manifest. If the status is @ACTIVE@, the
-- decoder manifest can\'t be edited. If the status is marked @DRAFT@, you
-- can edit the decoder manifest.
--
-- 'httpStatus', 'getDecoderManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getDecoderManifestResponse_name' - The name of the decoder manifest.
--
-- 'arn', 'getDecoderManifestResponse_arn' - The Amazon Resource Name (ARN) of the decoder manifest.
--
-- 'creationTime', 'getDecoderManifestResponse_creationTime' - The time the decoder manifest was created in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
--
-- 'lastModificationTime', 'getDecoderManifestResponse_lastModificationTime' - The time the decoder manifest was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
newGetDecoderManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  GetDecoderManifestResponse
newGetDecoderManifestResponse
  pHttpStatus_
  pName_
  pArn_
  pCreationTime_
  pLastModificationTime_ =
    GetDecoderManifestResponse'
      { description =
          Prelude.Nothing,
        modelManifestArn = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | A brief description of the decoder manifest.
getDecoderManifestResponse_description :: Lens.Lens' GetDecoderManifestResponse (Prelude.Maybe Prelude.Text)
getDecoderManifestResponse_description = Lens.lens (\GetDecoderManifestResponse' {description} -> description) (\s@GetDecoderManifestResponse' {} a -> s {description = a} :: GetDecoderManifestResponse)

-- | The ARN of a vehicle model (model manifest) associated with the decoder
-- manifest.
getDecoderManifestResponse_modelManifestArn :: Lens.Lens' GetDecoderManifestResponse (Prelude.Maybe Prelude.Text)
getDecoderManifestResponse_modelManifestArn = Lens.lens (\GetDecoderManifestResponse' {modelManifestArn} -> modelManifestArn) (\s@GetDecoderManifestResponse' {} a -> s {modelManifestArn = a} :: GetDecoderManifestResponse)

-- | The state of the decoder manifest. If the status is @ACTIVE@, the
-- decoder manifest can\'t be edited. If the status is marked @DRAFT@, you
-- can edit the decoder manifest.
getDecoderManifestResponse_status :: Lens.Lens' GetDecoderManifestResponse (Prelude.Maybe ManifestStatus)
getDecoderManifestResponse_status = Lens.lens (\GetDecoderManifestResponse' {status} -> status) (\s@GetDecoderManifestResponse' {} a -> s {status = a} :: GetDecoderManifestResponse)

-- | The response's http status code.
getDecoderManifestResponse_httpStatus :: Lens.Lens' GetDecoderManifestResponse Prelude.Int
getDecoderManifestResponse_httpStatus = Lens.lens (\GetDecoderManifestResponse' {httpStatus} -> httpStatus) (\s@GetDecoderManifestResponse' {} a -> s {httpStatus = a} :: GetDecoderManifestResponse)

-- | The name of the decoder manifest.
getDecoderManifestResponse_name :: Lens.Lens' GetDecoderManifestResponse Prelude.Text
getDecoderManifestResponse_name = Lens.lens (\GetDecoderManifestResponse' {name} -> name) (\s@GetDecoderManifestResponse' {} a -> s {name = a} :: GetDecoderManifestResponse)

-- | The Amazon Resource Name (ARN) of the decoder manifest.
getDecoderManifestResponse_arn :: Lens.Lens' GetDecoderManifestResponse Prelude.Text
getDecoderManifestResponse_arn = Lens.lens (\GetDecoderManifestResponse' {arn} -> arn) (\s@GetDecoderManifestResponse' {} a -> s {arn = a} :: GetDecoderManifestResponse)

-- | The time the decoder manifest was created in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
getDecoderManifestResponse_creationTime :: Lens.Lens' GetDecoderManifestResponse Prelude.UTCTime
getDecoderManifestResponse_creationTime = Lens.lens (\GetDecoderManifestResponse' {creationTime} -> creationTime) (\s@GetDecoderManifestResponse' {} a -> s {creationTime = a} :: GetDecoderManifestResponse) Prelude.. Data._Time

-- | The time the decoder manifest was last updated in seconds since epoch
-- (January 1, 1970 at midnight UTC time).
getDecoderManifestResponse_lastModificationTime :: Lens.Lens' GetDecoderManifestResponse Prelude.UTCTime
getDecoderManifestResponse_lastModificationTime = Lens.lens (\GetDecoderManifestResponse' {lastModificationTime} -> lastModificationTime) (\s@GetDecoderManifestResponse' {} a -> s {lastModificationTime = a} :: GetDecoderManifestResponse) Prelude.. Data._Time

instance Prelude.NFData GetDecoderManifestResponse where
  rnf GetDecoderManifestResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf modelManifestArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
