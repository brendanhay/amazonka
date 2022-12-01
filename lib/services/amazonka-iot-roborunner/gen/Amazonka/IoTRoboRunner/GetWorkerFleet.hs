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
-- Module      : Amazonka.IoTRoboRunner.GetWorkerFleet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to get a worker fleet
module Amazonka.IoTRoboRunner.GetWorkerFleet
  ( -- * Creating a Request
    GetWorkerFleet (..),
    newGetWorkerFleet,

    -- * Request Lenses
    getWorkerFleet_id,

    -- * Destructuring the Response
    GetWorkerFleetResponse (..),
    newGetWorkerFleetResponse,

    -- * Response Lenses
    getWorkerFleetResponse_additionalFixedProperties,
    getWorkerFleetResponse_httpStatus,
    getWorkerFleetResponse_id,
    getWorkerFleetResponse_arn,
    getWorkerFleetResponse_name,
    getWorkerFleetResponse_site,
    getWorkerFleetResponse_createdAt,
    getWorkerFleetResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWorkerFleet' smart constructor.
data GetWorkerFleet = GetWorkerFleet'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkerFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getWorkerFleet_id' - Undocumented member.
newGetWorkerFleet ::
  -- | 'id'
  Prelude.Text ->
  GetWorkerFleet
newGetWorkerFleet pId_ = GetWorkerFleet' {id = pId_}

-- | Undocumented member.
getWorkerFleet_id :: Lens.Lens' GetWorkerFleet Prelude.Text
getWorkerFleet_id = Lens.lens (\GetWorkerFleet' {id} -> id) (\s@GetWorkerFleet' {} a -> s {id = a} :: GetWorkerFleet)

instance Core.AWSRequest GetWorkerFleet where
  type
    AWSResponse GetWorkerFleet =
      GetWorkerFleetResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWorkerFleetResponse'
            Prelude.<$> (x Core..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "name")
            Prelude.<*> (x Core..:> "site")
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "updatedAt")
      )

instance Prelude.Hashable GetWorkerFleet where
  hashWithSalt _salt GetWorkerFleet' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetWorkerFleet where
  rnf GetWorkerFleet' {..} = Prelude.rnf id

instance Core.ToHeaders GetWorkerFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetWorkerFleet where
  toPath = Prelude.const "/getWorkerFleet"

instance Core.ToQuery GetWorkerFleet where
  toQuery GetWorkerFleet' {..} =
    Prelude.mconcat ["id" Core.=: id]

-- | /See:/ 'newGetWorkerFleetResponse' smart constructor.
data GetWorkerFleetResponse = GetWorkerFleetResponse'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    id :: Prelude.Text,
    arn :: Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text,
    createdAt :: Core.POSIX,
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWorkerFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'getWorkerFleetResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'getWorkerFleetResponse_httpStatus' - The response's http status code.
--
-- 'id', 'getWorkerFleetResponse_id' - Undocumented member.
--
-- 'arn', 'getWorkerFleetResponse_arn' - Undocumented member.
--
-- 'name', 'getWorkerFleetResponse_name' - Undocumented member.
--
-- 'site', 'getWorkerFleetResponse_site' - Undocumented member.
--
-- 'createdAt', 'getWorkerFleetResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'getWorkerFleetResponse_updatedAt' - Undocumented member.
newGetWorkerFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  GetWorkerFleetResponse
newGetWorkerFleetResponse
  pHttpStatus_
  pId_
  pArn_
  pName_
  pSite_
  pCreatedAt_
  pUpdatedAt_ =
    GetWorkerFleetResponse'
      { additionalFixedProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        id = pId_,
        arn = pArn_,
        name = pName_,
        site = pSite_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | Undocumented member.
getWorkerFleetResponse_additionalFixedProperties :: Lens.Lens' GetWorkerFleetResponse (Prelude.Maybe Prelude.Text)
getWorkerFleetResponse_additionalFixedProperties = Lens.lens (\GetWorkerFleetResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@GetWorkerFleetResponse' {} a -> s {additionalFixedProperties = a} :: GetWorkerFleetResponse)

-- | The response's http status code.
getWorkerFleetResponse_httpStatus :: Lens.Lens' GetWorkerFleetResponse Prelude.Int
getWorkerFleetResponse_httpStatus = Lens.lens (\GetWorkerFleetResponse' {httpStatus} -> httpStatus) (\s@GetWorkerFleetResponse' {} a -> s {httpStatus = a} :: GetWorkerFleetResponse)

-- | Undocumented member.
getWorkerFleetResponse_id :: Lens.Lens' GetWorkerFleetResponse Prelude.Text
getWorkerFleetResponse_id = Lens.lens (\GetWorkerFleetResponse' {id} -> id) (\s@GetWorkerFleetResponse' {} a -> s {id = a} :: GetWorkerFleetResponse)

-- | Undocumented member.
getWorkerFleetResponse_arn :: Lens.Lens' GetWorkerFleetResponse Prelude.Text
getWorkerFleetResponse_arn = Lens.lens (\GetWorkerFleetResponse' {arn} -> arn) (\s@GetWorkerFleetResponse' {} a -> s {arn = a} :: GetWorkerFleetResponse)

-- | Undocumented member.
getWorkerFleetResponse_name :: Lens.Lens' GetWorkerFleetResponse Prelude.Text
getWorkerFleetResponse_name = Lens.lens (\GetWorkerFleetResponse' {name} -> name) (\s@GetWorkerFleetResponse' {} a -> s {name = a} :: GetWorkerFleetResponse)

-- | Undocumented member.
getWorkerFleetResponse_site :: Lens.Lens' GetWorkerFleetResponse Prelude.Text
getWorkerFleetResponse_site = Lens.lens (\GetWorkerFleetResponse' {site} -> site) (\s@GetWorkerFleetResponse' {} a -> s {site = a} :: GetWorkerFleetResponse)

-- | Undocumented member.
getWorkerFleetResponse_createdAt :: Lens.Lens' GetWorkerFleetResponse Prelude.UTCTime
getWorkerFleetResponse_createdAt = Lens.lens (\GetWorkerFleetResponse' {createdAt} -> createdAt) (\s@GetWorkerFleetResponse' {} a -> s {createdAt = a} :: GetWorkerFleetResponse) Prelude.. Core._Time

-- | Undocumented member.
getWorkerFleetResponse_updatedAt :: Lens.Lens' GetWorkerFleetResponse Prelude.UTCTime
getWorkerFleetResponse_updatedAt = Lens.lens (\GetWorkerFleetResponse' {updatedAt} -> updatedAt) (\s@GetWorkerFleetResponse' {} a -> s {updatedAt = a} :: GetWorkerFleetResponse) Prelude.. Core._Time

instance Prelude.NFData GetWorkerFleetResponse where
  rnf GetWorkerFleetResponse' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf site
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
