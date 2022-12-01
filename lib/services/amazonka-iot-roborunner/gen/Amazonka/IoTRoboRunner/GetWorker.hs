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
-- Module      : Amazonka.IoTRoboRunner.GetWorker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to get a worker
module Amazonka.IoTRoboRunner.GetWorker
  ( -- * Creating a Request
    GetWorker (..),
    newGetWorker,

    -- * Request Lenses
    getWorker_id,

    -- * Destructuring the Response
    GetWorkerResponse (..),
    newGetWorkerResponse,

    -- * Response Lenses
    getWorkerResponse_additionalTransientProperties,
    getWorkerResponse_orientation,
    getWorkerResponse_vendorProperties,
    getWorkerResponse_position,
    getWorkerResponse_additionalFixedProperties,
    getWorkerResponse_httpStatus,
    getWorkerResponse_arn,
    getWorkerResponse_id,
    getWorkerResponse_fleet,
    getWorkerResponse_site,
    getWorkerResponse_createdAt,
    getWorkerResponse_updatedAt,
    getWorkerResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorker' smart constructor.
data GetWorker = GetWorker'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWorker_id' - Undocumented member.
newGetWorker ::
  -- | 'id'
  Prelude.Text ->
  GetWorker
newGetWorker pId_ = GetWorker' {id = pId_}

-- | Undocumented member.
getWorker_id :: Lens.Lens' GetWorker Prelude.Text
getWorker_id = Lens.lens (\GetWorker' {id} -> id) (\s@GetWorker' {} a -> s {id = a} :: GetWorker)

instance Core.AWSRequest GetWorker where
  type AWSResponse GetWorker = GetWorkerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkerResponse'
            Prelude.<$> (x Core..?> "additionalTransientProperties")
            Prelude.<*> (x Core..?> "orientation")
            Prelude.<*> (x Core..?> "vendorProperties")
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (x Core..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "fleet")
            Prelude.<*> (x Core..:> "site")
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "updatedAt")
            Prelude.<*> (x Core..:> "name")
      )

instance Prelude.Hashable GetWorker where
  hashWithSalt _salt GetWorker' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorker where
  rnf GetWorker' {..} = Prelude.rnf id

instance Core.ToHeaders GetWorker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWorker where
  toPath = Prelude.const "/getWorker"

instance Core.ToQuery GetWorker where
  toQuery GetWorker' {..} =
    Prelude.mconcat ["id" Core.=: id]

-- | /See:/ 'newGetWorkerResponse' smart constructor.
data GetWorkerResponse = GetWorkerResponse'
  { additionalTransientProperties :: Prelude.Maybe Prelude.Text,
    orientation :: Prelude.Maybe Orientation,
    vendorProperties :: Prelude.Maybe VendorProperties,
    position :: Prelude.Maybe PositionCoordinates,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    fleet :: Prelude.Text,
    site :: Prelude.Text,
    createdAt :: Core.POSIX,
    updatedAt :: Core.POSIX,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalTransientProperties', 'getWorkerResponse_additionalTransientProperties' - Undocumented member.
--
-- 'orientation', 'getWorkerResponse_orientation' - Undocumented member.
--
-- 'vendorProperties', 'getWorkerResponse_vendorProperties' - Undocumented member.
--
-- 'position', 'getWorkerResponse_position' - Undocumented member.
--
-- 'additionalFixedProperties', 'getWorkerResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'getWorkerResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getWorkerResponse_arn' - Undocumented member.
--
-- 'id', 'getWorkerResponse_id' - Undocumented member.
--
-- 'fleet', 'getWorkerResponse_fleet' - Undocumented member.
--
-- 'site', 'getWorkerResponse_site' - Undocumented member.
--
-- 'createdAt', 'getWorkerResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'getWorkerResponse_updatedAt' - Undocumented member.
--
-- 'name', 'getWorkerResponse_name' - Undocumented member.
newGetWorkerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  GetWorkerResponse
newGetWorkerResponse
  pHttpStatus_
  pArn_
  pId_
  pFleet_
  pSite_
  pCreatedAt_
  pUpdatedAt_
  pName_ =
    GetWorkerResponse'
      { additionalTransientProperties =
          Prelude.Nothing,
        orientation = Prelude.Nothing,
        vendorProperties = Prelude.Nothing,
        position = Prelude.Nothing,
        additionalFixedProperties = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        fleet = pFleet_,
        site = pSite_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        name = pName_
      }

-- | Undocumented member.
getWorkerResponse_additionalTransientProperties :: Lens.Lens' GetWorkerResponse (Prelude.Maybe Prelude.Text)
getWorkerResponse_additionalTransientProperties = Lens.lens (\GetWorkerResponse' {additionalTransientProperties} -> additionalTransientProperties) (\s@GetWorkerResponse' {} a -> s {additionalTransientProperties = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_orientation :: Lens.Lens' GetWorkerResponse (Prelude.Maybe Orientation)
getWorkerResponse_orientation = Lens.lens (\GetWorkerResponse' {orientation} -> orientation) (\s@GetWorkerResponse' {} a -> s {orientation = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_vendorProperties :: Lens.Lens' GetWorkerResponse (Prelude.Maybe VendorProperties)
getWorkerResponse_vendorProperties = Lens.lens (\GetWorkerResponse' {vendorProperties} -> vendorProperties) (\s@GetWorkerResponse' {} a -> s {vendorProperties = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_position :: Lens.Lens' GetWorkerResponse (Prelude.Maybe PositionCoordinates)
getWorkerResponse_position = Lens.lens (\GetWorkerResponse' {position} -> position) (\s@GetWorkerResponse' {} a -> s {position = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_additionalFixedProperties :: Lens.Lens' GetWorkerResponse (Prelude.Maybe Prelude.Text)
getWorkerResponse_additionalFixedProperties = Lens.lens (\GetWorkerResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@GetWorkerResponse' {} a -> s {additionalFixedProperties = a} :: GetWorkerResponse)

-- | The response's http status code.
getWorkerResponse_httpStatus :: Lens.Lens' GetWorkerResponse Prelude.Int
getWorkerResponse_httpStatus = Lens.lens (\GetWorkerResponse' {httpStatus} -> httpStatus) (\s@GetWorkerResponse' {} a -> s {httpStatus = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_arn :: Lens.Lens' GetWorkerResponse Prelude.Text
getWorkerResponse_arn = Lens.lens (\GetWorkerResponse' {arn} -> arn) (\s@GetWorkerResponse' {} a -> s {arn = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_id :: Lens.Lens' GetWorkerResponse Prelude.Text
getWorkerResponse_id = Lens.lens (\GetWorkerResponse' {id} -> id) (\s@GetWorkerResponse' {} a -> s {id = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_fleet :: Lens.Lens' GetWorkerResponse Prelude.Text
getWorkerResponse_fleet = Lens.lens (\GetWorkerResponse' {fleet} -> fleet) (\s@GetWorkerResponse' {} a -> s {fleet = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_site :: Lens.Lens' GetWorkerResponse Prelude.Text
getWorkerResponse_site = Lens.lens (\GetWorkerResponse' {site} -> site) (\s@GetWorkerResponse' {} a -> s {site = a} :: GetWorkerResponse)

-- | Undocumented member.
getWorkerResponse_createdAt :: Lens.Lens' GetWorkerResponse Prelude.UTCTime
getWorkerResponse_createdAt = Lens.lens (\GetWorkerResponse' {createdAt} -> createdAt) (\s@GetWorkerResponse' {} a -> s {createdAt = a} :: GetWorkerResponse) Prelude.. Core._Time

-- | Undocumented member.
getWorkerResponse_updatedAt :: Lens.Lens' GetWorkerResponse Prelude.UTCTime
getWorkerResponse_updatedAt = Lens.lens (\GetWorkerResponse' {updatedAt} -> updatedAt) (\s@GetWorkerResponse' {} a -> s {updatedAt = a} :: GetWorkerResponse) Prelude.. Core._Time

-- | Undocumented member.
getWorkerResponse_name :: Lens.Lens' GetWorkerResponse Prelude.Text
getWorkerResponse_name = Lens.lens (\GetWorkerResponse' {name} -> name) (\s@GetWorkerResponse' {} a -> s {name = a} :: GetWorkerResponse)

instance Prelude.NFData GetWorkerResponse where
  rnf GetWorkerResponse' {..} =
    Prelude.rnf additionalTransientProperties
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf vendorProperties
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf fleet
      `Prelude.seq` Prelude.rnf site
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf name
