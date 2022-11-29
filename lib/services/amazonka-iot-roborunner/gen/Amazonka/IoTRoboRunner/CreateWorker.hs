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
-- Module      : Amazonka.IoTRoboRunner.CreateWorker
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to create a worker
module Amazonka.IoTRoboRunner.CreateWorker
  ( -- * Creating a Request
    CreateWorker (..),
    newCreateWorker,

    -- * Request Lenses
    createWorker_clientToken,
    createWorker_additionalTransientProperties,
    createWorker_orientation,
    createWorker_vendorProperties,
    createWorker_position,
    createWorker_additionalFixedProperties,
    createWorker_name,
    createWorker_fleet,

    -- * Destructuring the Response
    CreateWorkerResponse (..),
    newCreateWorkerResponse,

    -- * Response Lenses
    createWorkerResponse_httpStatus,
    createWorkerResponse_arn,
    createWorkerResponse_id,
    createWorkerResponse_createdAt,
    createWorkerResponse_updatedAt,
    createWorkerResponse_site,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWorker' smart constructor.
data CreateWorker = CreateWorker'
  { clientToken :: Prelude.Maybe Prelude.Text,
    additionalTransientProperties :: Prelude.Maybe Prelude.Text,
    orientation :: Prelude.Maybe Orientation,
    vendorProperties :: Prelude.Maybe VendorProperties,
    position :: Prelude.Maybe PositionCoordinates,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Text,
    fleet :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createWorker_clientToken' - Undocumented member.
--
-- 'additionalTransientProperties', 'createWorker_additionalTransientProperties' - Undocumented member.
--
-- 'orientation', 'createWorker_orientation' - Undocumented member.
--
-- 'vendorProperties', 'createWorker_vendorProperties' - Undocumented member.
--
-- 'position', 'createWorker_position' - Undocumented member.
--
-- 'additionalFixedProperties', 'createWorker_additionalFixedProperties' - Undocumented member.
--
-- 'name', 'createWorker_name' - Undocumented member.
--
-- 'fleet', 'createWorker_fleet' - Undocumented member.
newCreateWorker ::
  -- | 'name'
  Prelude.Text ->
  -- | 'fleet'
  Prelude.Text ->
  CreateWorker
newCreateWorker pName_ pFleet_ =
  CreateWorker'
    { clientToken = Prelude.Nothing,
      additionalTransientProperties = Prelude.Nothing,
      orientation = Prelude.Nothing,
      vendorProperties = Prelude.Nothing,
      position = Prelude.Nothing,
      additionalFixedProperties = Prelude.Nothing,
      name = pName_,
      fleet = pFleet_
    }

-- | Undocumented member.
createWorker_clientToken :: Lens.Lens' CreateWorker (Prelude.Maybe Prelude.Text)
createWorker_clientToken = Lens.lens (\CreateWorker' {clientToken} -> clientToken) (\s@CreateWorker' {} a -> s {clientToken = a} :: CreateWorker)

-- | Undocumented member.
createWorker_additionalTransientProperties :: Lens.Lens' CreateWorker (Prelude.Maybe Prelude.Text)
createWorker_additionalTransientProperties = Lens.lens (\CreateWorker' {additionalTransientProperties} -> additionalTransientProperties) (\s@CreateWorker' {} a -> s {additionalTransientProperties = a} :: CreateWorker)

-- | Undocumented member.
createWorker_orientation :: Lens.Lens' CreateWorker (Prelude.Maybe Orientation)
createWorker_orientation = Lens.lens (\CreateWorker' {orientation} -> orientation) (\s@CreateWorker' {} a -> s {orientation = a} :: CreateWorker)

-- | Undocumented member.
createWorker_vendorProperties :: Lens.Lens' CreateWorker (Prelude.Maybe VendorProperties)
createWorker_vendorProperties = Lens.lens (\CreateWorker' {vendorProperties} -> vendorProperties) (\s@CreateWorker' {} a -> s {vendorProperties = a} :: CreateWorker)

-- | Undocumented member.
createWorker_position :: Lens.Lens' CreateWorker (Prelude.Maybe PositionCoordinates)
createWorker_position = Lens.lens (\CreateWorker' {position} -> position) (\s@CreateWorker' {} a -> s {position = a} :: CreateWorker)

-- | Undocumented member.
createWorker_additionalFixedProperties :: Lens.Lens' CreateWorker (Prelude.Maybe Prelude.Text)
createWorker_additionalFixedProperties = Lens.lens (\CreateWorker' {additionalFixedProperties} -> additionalFixedProperties) (\s@CreateWorker' {} a -> s {additionalFixedProperties = a} :: CreateWorker)

-- | Undocumented member.
createWorker_name :: Lens.Lens' CreateWorker Prelude.Text
createWorker_name = Lens.lens (\CreateWorker' {name} -> name) (\s@CreateWorker' {} a -> s {name = a} :: CreateWorker)

-- | Undocumented member.
createWorker_fleet :: Lens.Lens' CreateWorker Prelude.Text
createWorker_fleet = Lens.lens (\CreateWorker' {fleet} -> fleet) (\s@CreateWorker' {} a -> s {fleet = a} :: CreateWorker)

instance Core.AWSRequest CreateWorker where
  type AWSResponse CreateWorker = CreateWorkerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWorkerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "updatedAt")
            Prelude.<*> (x Core..:> "site")
      )

instance Prelude.Hashable CreateWorker where
  hashWithSalt _salt CreateWorker' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` additionalTransientProperties
      `Prelude.hashWithSalt` orientation
      `Prelude.hashWithSalt` vendorProperties
      `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` fleet

instance Prelude.NFData CreateWorker where
  rnf CreateWorker' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf additionalTransientProperties
      `Prelude.seq` Prelude.rnf orientation
      `Prelude.seq` Prelude.rnf vendorProperties
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf fleet

instance Core.ToHeaders CreateWorker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateWorker where
  toJSON CreateWorker' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("additionalTransientProperties" Core..=)
              Prelude.<$> additionalTransientProperties,
            ("orientation" Core..=) Prelude.<$> orientation,
            ("vendorProperties" Core..=)
              Prelude.<$> vendorProperties,
            ("position" Core..=) Prelude.<$> position,
            ("additionalFixedProperties" Core..=)
              Prelude.<$> additionalFixedProperties,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("fleet" Core..= fleet)
          ]
      )

instance Core.ToPath CreateWorker where
  toPath = Prelude.const "/createWorker"

instance Core.ToQuery CreateWorker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWorkerResponse' smart constructor.
data CreateWorkerResponse = CreateWorkerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    createdAt :: Core.POSIX,
    updatedAt :: Core.POSIX,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWorkerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createWorkerResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createWorkerResponse_arn' - Undocumented member.
--
-- 'id', 'createWorkerResponse_id' - Undocumented member.
--
-- 'createdAt', 'createWorkerResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'createWorkerResponse_updatedAt' - Undocumented member.
--
-- 'site', 'createWorkerResponse_site' - Undocumented member.
newCreateWorkerResponse ::
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
  -- | 'site'
  Prelude.Text ->
  CreateWorkerResponse
newCreateWorkerResponse
  pHttpStatus_
  pArn_
  pId_
  pCreatedAt_
  pUpdatedAt_
  pSite_ =
    CreateWorkerResponse'
      { httpStatus = pHttpStatus_,
        arn = pArn_,
        id = pId_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        site = pSite_
      }

-- | The response's http status code.
createWorkerResponse_httpStatus :: Lens.Lens' CreateWorkerResponse Prelude.Int
createWorkerResponse_httpStatus = Lens.lens (\CreateWorkerResponse' {httpStatus} -> httpStatus) (\s@CreateWorkerResponse' {} a -> s {httpStatus = a} :: CreateWorkerResponse)

-- | Undocumented member.
createWorkerResponse_arn :: Lens.Lens' CreateWorkerResponse Prelude.Text
createWorkerResponse_arn = Lens.lens (\CreateWorkerResponse' {arn} -> arn) (\s@CreateWorkerResponse' {} a -> s {arn = a} :: CreateWorkerResponse)

-- | Undocumented member.
createWorkerResponse_id :: Lens.Lens' CreateWorkerResponse Prelude.Text
createWorkerResponse_id = Lens.lens (\CreateWorkerResponse' {id} -> id) (\s@CreateWorkerResponse' {} a -> s {id = a} :: CreateWorkerResponse)

-- | Undocumented member.
createWorkerResponse_createdAt :: Lens.Lens' CreateWorkerResponse Prelude.UTCTime
createWorkerResponse_createdAt = Lens.lens (\CreateWorkerResponse' {createdAt} -> createdAt) (\s@CreateWorkerResponse' {} a -> s {createdAt = a} :: CreateWorkerResponse) Prelude.. Core._Time

-- | Undocumented member.
createWorkerResponse_updatedAt :: Lens.Lens' CreateWorkerResponse Prelude.UTCTime
createWorkerResponse_updatedAt = Lens.lens (\CreateWorkerResponse' {updatedAt} -> updatedAt) (\s@CreateWorkerResponse' {} a -> s {updatedAt = a} :: CreateWorkerResponse) Prelude.. Core._Time

-- | Undocumented member.
createWorkerResponse_site :: Lens.Lens' CreateWorkerResponse Prelude.Text
createWorkerResponse_site = Lens.lens (\CreateWorkerResponse' {site} -> site) (\s@CreateWorkerResponse' {} a -> s {site = a} :: CreateWorkerResponse)

instance Prelude.NFData CreateWorkerResponse where
  rnf CreateWorkerResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf site
