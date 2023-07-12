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
-- Module      : Amazonka.IoTFleetWise.GetFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet.
module Amazonka.IoTFleetWise.GetFleet
  ( -- * Creating a Request
    GetFleet (..),
    newGetFleet,

    -- * Request Lenses
    getFleet_fleetId,

    -- * Destructuring the Response
    GetFleetResponse (..),
    newGetFleetResponse,

    -- * Response Lenses
    getFleetResponse_description,
    getFleetResponse_httpStatus,
    getFleetResponse_id,
    getFleetResponse_arn,
    getFleetResponse_signalCatalogArn,
    getFleetResponse_creationTime,
    getFleetResponse_lastModificationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetFleet' smart constructor.
data GetFleet = GetFleet'
  { -- | The ID of the fleet to retrieve information about.
    fleetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'getFleet_fleetId' - The ID of the fleet to retrieve information about.
newGetFleet ::
  -- | 'fleetId'
  Prelude.Text ->
  GetFleet
newGetFleet pFleetId_ =
  GetFleet' {fleetId = pFleetId_}

-- | The ID of the fleet to retrieve information about.
getFleet_fleetId :: Lens.Lens' GetFleet Prelude.Text
getFleet_fleetId = Lens.lens (\GetFleet' {fleetId} -> fleetId) (\s@GetFleet' {} a -> s {fleetId = a} :: GetFleet)

instance Core.AWSRequest GetFleet where
  type AWSResponse GetFleet = GetFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFleetResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "signalCatalogArn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "lastModificationTime")
      )

instance Prelude.Hashable GetFleet where
  hashWithSalt _salt GetFleet' {..} =
    _salt `Prelude.hashWithSalt` fleetId

instance Prelude.NFData GetFleet where
  rnf GetFleet' {..} = Prelude.rnf fleetId

instance Data.ToHeaders GetFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetFleet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetFleet where
  toJSON GetFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("fleetId" Data..= fleetId)]
      )

instance Data.ToPath GetFleet where
  toPath = Prelude.const "/"

instance Data.ToQuery GetFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFleetResponse' smart constructor.
data GetFleetResponse = GetFleetResponse'
  { -- | A brief description of the fleet.
    description :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the fleet.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the fleet.
    arn :: Prelude.Text,
    -- | The ARN of a signal catalog associated with the fleet.
    signalCatalogArn :: Prelude.Text,
    -- | The time the fleet was created in seconds since epoch (January 1, 1970
    -- at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The time the fleet was last updated, in seconds since epoch (January 1,
    -- 1970 at midnight UTC time).
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getFleetResponse_description' - A brief description of the fleet.
--
-- 'httpStatus', 'getFleetResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getFleetResponse_id' - The ID of the fleet.
--
-- 'arn', 'getFleetResponse_arn' - The Amazon Resource Name (ARN) of the fleet.
--
-- 'signalCatalogArn', 'getFleetResponse_signalCatalogArn' - The ARN of a signal catalog associated with the fleet.
--
-- 'creationTime', 'getFleetResponse_creationTime' - The time the fleet was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
--
-- 'lastModificationTime', 'getFleetResponse_lastModificationTime' - The time the fleet was last updated, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
newGetFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'signalCatalogArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  GetFleetResponse
newGetFleetResponse
  pHttpStatus_
  pId_
  pArn_
  pSignalCatalogArn_
  pCreationTime_
  pLastModificationTime_ =
    GetFleetResponse'
      { description = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        arn = pArn_,
        signalCatalogArn = pSignalCatalogArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | A brief description of the fleet.
getFleetResponse_description :: Lens.Lens' GetFleetResponse (Prelude.Maybe Prelude.Text)
getFleetResponse_description = Lens.lens (\GetFleetResponse' {description} -> description) (\s@GetFleetResponse' {} a -> s {description = a} :: GetFleetResponse)

-- | The response's http status code.
getFleetResponse_httpStatus :: Lens.Lens' GetFleetResponse Prelude.Int
getFleetResponse_httpStatus = Lens.lens (\GetFleetResponse' {httpStatus} -> httpStatus) (\s@GetFleetResponse' {} a -> s {httpStatus = a} :: GetFleetResponse)

-- | The ID of the fleet.
getFleetResponse_id :: Lens.Lens' GetFleetResponse Prelude.Text
getFleetResponse_id = Lens.lens (\GetFleetResponse' {id} -> id) (\s@GetFleetResponse' {} a -> s {id = a} :: GetFleetResponse)

-- | The Amazon Resource Name (ARN) of the fleet.
getFleetResponse_arn :: Lens.Lens' GetFleetResponse Prelude.Text
getFleetResponse_arn = Lens.lens (\GetFleetResponse' {arn} -> arn) (\s@GetFleetResponse' {} a -> s {arn = a} :: GetFleetResponse)

-- | The ARN of a signal catalog associated with the fleet.
getFleetResponse_signalCatalogArn :: Lens.Lens' GetFleetResponse Prelude.Text
getFleetResponse_signalCatalogArn = Lens.lens (\GetFleetResponse' {signalCatalogArn} -> signalCatalogArn) (\s@GetFleetResponse' {} a -> s {signalCatalogArn = a} :: GetFleetResponse)

-- | The time the fleet was created in seconds since epoch (January 1, 1970
-- at midnight UTC time).
getFleetResponse_creationTime :: Lens.Lens' GetFleetResponse Prelude.UTCTime
getFleetResponse_creationTime = Lens.lens (\GetFleetResponse' {creationTime} -> creationTime) (\s@GetFleetResponse' {} a -> s {creationTime = a} :: GetFleetResponse) Prelude.. Data._Time

-- | The time the fleet was last updated, in seconds since epoch (January 1,
-- 1970 at midnight UTC time).
getFleetResponse_lastModificationTime :: Lens.Lens' GetFleetResponse Prelude.UTCTime
getFleetResponse_lastModificationTime = Lens.lens (\GetFleetResponse' {lastModificationTime} -> lastModificationTime) (\s@GetFleetResponse' {} a -> s {lastModificationTime = a} :: GetFleetResponse) Prelude.. Data._Time

instance Prelude.NFData GetFleetResponse where
  rnf GetFleetResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
