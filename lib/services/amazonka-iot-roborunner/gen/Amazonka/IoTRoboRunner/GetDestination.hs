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
-- Module      : Amazonka.IoTRoboRunner.GetDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to get a destination
module Amazonka.IoTRoboRunner.GetDestination
  ( -- * Creating a Request
    GetDestination (..),
    newGetDestination,

    -- * Request Lenses
    getDestination_id,

    -- * Destructuring the Response
    GetDestinationResponse (..),
    newGetDestinationResponse,

    -- * Response Lenses
    getDestinationResponse_additionalFixedProperties,
    getDestinationResponse_httpStatus,
    getDestinationResponse_arn,
    getDestinationResponse_id,
    getDestinationResponse_name,
    getDestinationResponse_site,
    getDestinationResponse_createdAt,
    getDestinationResponse_updatedAt,
    getDestinationResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDestination' smart constructor.
data GetDestination = GetDestination'
  { id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getDestination_id' - Undocumented member.
newGetDestination ::
  -- | 'id'
  Prelude.Text ->
  GetDestination
newGetDestination pId_ = GetDestination' {id = pId_}

-- | Undocumented member.
getDestination_id :: Lens.Lens' GetDestination Prelude.Text
getDestination_id = Lens.lens (\GetDestination' {id} -> id) (\s@GetDestination' {} a -> s {id = a} :: GetDestination)

instance Core.AWSRequest GetDestination where
  type
    AWSResponse GetDestination =
      GetDestinationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDestinationResponse'
            Prelude.<$> (x Data..?> "additionalFixedProperties")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "site")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "updatedAt")
            Prelude.<*> (x Data..:> "state")
      )

instance Prelude.Hashable GetDestination where
  hashWithSalt _salt GetDestination' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetDestination where
  rnf GetDestination' {..} = Prelude.rnf id

instance Data.ToHeaders GetDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetDestination where
  toPath = Prelude.const "/getDestination"

instance Data.ToQuery GetDestination where
  toQuery GetDestination' {..} =
    Prelude.mconcat ["id" Data.=: id]

-- | /See:/ 'newGetDestinationResponse' smart constructor.
data GetDestinationResponse = GetDestinationResponse'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX,
    state :: DestinationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'getDestinationResponse_additionalFixedProperties' - Undocumented member.
--
-- 'httpStatus', 'getDestinationResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getDestinationResponse_arn' - Undocumented member.
--
-- 'id', 'getDestinationResponse_id' - Undocumented member.
--
-- 'name', 'getDestinationResponse_name' - Undocumented member.
--
-- 'site', 'getDestinationResponse_site' - Undocumented member.
--
-- 'createdAt', 'getDestinationResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'getDestinationResponse_updatedAt' - Undocumented member.
--
-- 'state', 'getDestinationResponse_state' - Undocumented member.
newGetDestinationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'state'
  DestinationState ->
  GetDestinationResponse
newGetDestinationResponse
  pHttpStatus_
  pArn_
  pId_
  pName_
  pSite_
  pCreatedAt_
  pUpdatedAt_
  pState_ =
    GetDestinationResponse'
      { additionalFixedProperties =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        name = pName_,
        site = pSite_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        state = pState_
      }

-- | Undocumented member.
getDestinationResponse_additionalFixedProperties :: Lens.Lens' GetDestinationResponse (Prelude.Maybe Prelude.Text)
getDestinationResponse_additionalFixedProperties = Lens.lens (\GetDestinationResponse' {additionalFixedProperties} -> additionalFixedProperties) (\s@GetDestinationResponse' {} a -> s {additionalFixedProperties = a} :: GetDestinationResponse)

-- | The response's http status code.
getDestinationResponse_httpStatus :: Lens.Lens' GetDestinationResponse Prelude.Int
getDestinationResponse_httpStatus = Lens.lens (\GetDestinationResponse' {httpStatus} -> httpStatus) (\s@GetDestinationResponse' {} a -> s {httpStatus = a} :: GetDestinationResponse)

-- | Undocumented member.
getDestinationResponse_arn :: Lens.Lens' GetDestinationResponse Prelude.Text
getDestinationResponse_arn = Lens.lens (\GetDestinationResponse' {arn} -> arn) (\s@GetDestinationResponse' {} a -> s {arn = a} :: GetDestinationResponse)

-- | Undocumented member.
getDestinationResponse_id :: Lens.Lens' GetDestinationResponse Prelude.Text
getDestinationResponse_id = Lens.lens (\GetDestinationResponse' {id} -> id) (\s@GetDestinationResponse' {} a -> s {id = a} :: GetDestinationResponse)

-- | Undocumented member.
getDestinationResponse_name :: Lens.Lens' GetDestinationResponse Prelude.Text
getDestinationResponse_name = Lens.lens (\GetDestinationResponse' {name} -> name) (\s@GetDestinationResponse' {} a -> s {name = a} :: GetDestinationResponse)

-- | Undocumented member.
getDestinationResponse_site :: Lens.Lens' GetDestinationResponse Prelude.Text
getDestinationResponse_site = Lens.lens (\GetDestinationResponse' {site} -> site) (\s@GetDestinationResponse' {} a -> s {site = a} :: GetDestinationResponse)

-- | Undocumented member.
getDestinationResponse_createdAt :: Lens.Lens' GetDestinationResponse Prelude.UTCTime
getDestinationResponse_createdAt = Lens.lens (\GetDestinationResponse' {createdAt} -> createdAt) (\s@GetDestinationResponse' {} a -> s {createdAt = a} :: GetDestinationResponse) Prelude.. Data._Time

-- | Undocumented member.
getDestinationResponse_updatedAt :: Lens.Lens' GetDestinationResponse Prelude.UTCTime
getDestinationResponse_updatedAt = Lens.lens (\GetDestinationResponse' {updatedAt} -> updatedAt) (\s@GetDestinationResponse' {} a -> s {updatedAt = a} :: GetDestinationResponse) Prelude.. Data._Time

-- | Undocumented member.
getDestinationResponse_state :: Lens.Lens' GetDestinationResponse DestinationState
getDestinationResponse_state = Lens.lens (\GetDestinationResponse' {state} -> state) (\s@GetDestinationResponse' {} a -> s {state = a} :: GetDestinationResponse)

instance Prelude.NFData GetDestinationResponse where
  rnf GetDestinationResponse' {..} =
    Prelude.rnf additionalFixedProperties `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf arn `Prelude.seq`
          Prelude.rnf id `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf site `Prelude.seq`
                Prelude.rnf createdAt `Prelude.seq`
                  Prelude.rnf updatedAt `Prelude.seq`
                    Prelude.rnf state
