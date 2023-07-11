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
-- Module      : Amazonka.IoTRoboRunner.CreateWorkerFleet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to create a worker fleet
module Amazonka.IoTRoboRunner.CreateWorkerFleet
  ( -- * Creating a Request
    CreateWorkerFleet (..),
    newCreateWorkerFleet,

    -- * Request Lenses
    createWorkerFleet_additionalFixedProperties,
    createWorkerFleet_clientToken,
    createWorkerFleet_name,
    createWorkerFleet_site,

    -- * Destructuring the Response
    CreateWorkerFleetResponse (..),
    newCreateWorkerFleetResponse,

    -- * Response Lenses
    createWorkerFleetResponse_httpStatus,
    createWorkerFleetResponse_arn,
    createWorkerFleetResponse_id,
    createWorkerFleetResponse_createdAt,
    createWorkerFleetResponse_updatedAt,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorkerFleet' smart constructor.
data CreateWorkerFleet = CreateWorkerFleet'
  { additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    clientToken :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkerFleet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalFixedProperties', 'createWorkerFleet_additionalFixedProperties' - Undocumented member.
--
-- 'clientToken', 'createWorkerFleet_clientToken' - Undocumented member.
--
-- 'name', 'createWorkerFleet_name' - Undocumented member.
--
-- 'site', 'createWorkerFleet_site' - Undocumented member.
newCreateWorkerFleet ::
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  CreateWorkerFleet
newCreateWorkerFleet pName_ pSite_ =
  CreateWorkerFleet'
    { additionalFixedProperties =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      name = pName_,
      site = pSite_
    }

-- | Undocumented member.
createWorkerFleet_additionalFixedProperties :: Lens.Lens' CreateWorkerFleet (Prelude.Maybe Prelude.Text)
createWorkerFleet_additionalFixedProperties = Lens.lens (\CreateWorkerFleet' {additionalFixedProperties} -> additionalFixedProperties) (\s@CreateWorkerFleet' {} a -> s {additionalFixedProperties = a} :: CreateWorkerFleet)

-- | Undocumented member.
createWorkerFleet_clientToken :: Lens.Lens' CreateWorkerFleet (Prelude.Maybe Prelude.Text)
createWorkerFleet_clientToken = Lens.lens (\CreateWorkerFleet' {clientToken} -> clientToken) (\s@CreateWorkerFleet' {} a -> s {clientToken = a} :: CreateWorkerFleet)

-- | Undocumented member.
createWorkerFleet_name :: Lens.Lens' CreateWorkerFleet Prelude.Text
createWorkerFleet_name = Lens.lens (\CreateWorkerFleet' {name} -> name) (\s@CreateWorkerFleet' {} a -> s {name = a} :: CreateWorkerFleet)

-- | Undocumented member.
createWorkerFleet_site :: Lens.Lens' CreateWorkerFleet Prelude.Text
createWorkerFleet_site = Lens.lens (\CreateWorkerFleet' {site} -> site) (\s@CreateWorkerFleet' {} a -> s {site = a} :: CreateWorkerFleet)

instance Core.AWSRequest CreateWorkerFleet where
  type
    AWSResponse CreateWorkerFleet =
      CreateWorkerFleetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkerFleetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "id")
            Prelude.<*> (x Data..:> "createdAt")
            Prelude.<*> (x Data..:> "updatedAt")
      )

instance Prelude.Hashable CreateWorkerFleet where
  hashWithSalt _salt CreateWorkerFleet' {..} =
    _salt
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` site

instance Prelude.NFData CreateWorkerFleet where
  rnf CreateWorkerFleet' {..} =
    Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf site

instance Data.ToHeaders CreateWorkerFleet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWorkerFleet where
  toJSON CreateWorkerFleet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalFixedProperties" Data..=)
              Prelude.<$> additionalFixedProperties,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("site" Data..= site)
          ]
      )

instance Data.ToPath CreateWorkerFleet where
  toPath = Prelude.const "/createWorkerFleet"

instance Data.ToQuery CreateWorkerFleet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkerFleetResponse' smart constructor.
data CreateWorkerFleetResponse = CreateWorkerFleetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    createdAt :: Data.POSIX,
    updatedAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkerFleetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkerFleetResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createWorkerFleetResponse_arn' - Undocumented member.
--
-- 'id', 'createWorkerFleetResponse_id' - Undocumented member.
--
-- 'createdAt', 'createWorkerFleetResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'createWorkerFleetResponse_updatedAt' - Undocumented member.
newCreateWorkerFleetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  CreateWorkerFleetResponse
newCreateWorkerFleetResponse
  pHttpStatus_
  pArn_
  pId_
  pCreatedAt_
  pUpdatedAt_ =
    CreateWorkerFleetResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        id = pId_,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_
      }

-- | The response's http status code.
createWorkerFleetResponse_httpStatus :: Lens.Lens' CreateWorkerFleetResponse Prelude.Int
createWorkerFleetResponse_httpStatus = Lens.lens (\CreateWorkerFleetResponse' {httpStatus} -> httpStatus) (\s@CreateWorkerFleetResponse' {} a -> s {httpStatus = a} :: CreateWorkerFleetResponse)

-- | Undocumented member.
createWorkerFleetResponse_arn :: Lens.Lens' CreateWorkerFleetResponse Prelude.Text
createWorkerFleetResponse_arn = Lens.lens (\CreateWorkerFleetResponse' {arn} -> arn) (\s@CreateWorkerFleetResponse' {} a -> s {arn = a} :: CreateWorkerFleetResponse)

-- | Undocumented member.
createWorkerFleetResponse_id :: Lens.Lens' CreateWorkerFleetResponse Prelude.Text
createWorkerFleetResponse_id = Lens.lens (\CreateWorkerFleetResponse' {id} -> id) (\s@CreateWorkerFleetResponse' {} a -> s {id = a} :: CreateWorkerFleetResponse)

-- | Undocumented member.
createWorkerFleetResponse_createdAt :: Lens.Lens' CreateWorkerFleetResponse Prelude.UTCTime
createWorkerFleetResponse_createdAt = Lens.lens (\CreateWorkerFleetResponse' {createdAt} -> createdAt) (\s@CreateWorkerFleetResponse' {} a -> s {createdAt = a} :: CreateWorkerFleetResponse) Prelude.. Data._Time

-- | Undocumented member.
createWorkerFleetResponse_updatedAt :: Lens.Lens' CreateWorkerFleetResponse Prelude.UTCTime
createWorkerFleetResponse_updatedAt = Lens.lens (\CreateWorkerFleetResponse' {updatedAt} -> updatedAt) (\s@CreateWorkerFleetResponse' {} a -> s {updatedAt = a} :: CreateWorkerFleetResponse) Prelude.. Data._Time

instance Prelude.NFData CreateWorkerFleetResponse where
  rnf CreateWorkerFleetResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
