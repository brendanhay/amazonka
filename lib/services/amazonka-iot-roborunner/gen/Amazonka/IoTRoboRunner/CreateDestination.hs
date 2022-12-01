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
-- Module      : Amazonka.IoTRoboRunner.CreateDestination
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants permission to create a destination
module Amazonka.IoTRoboRunner.CreateDestination
  ( -- * Creating a Request
    CreateDestination (..),
    newCreateDestination,

    -- * Request Lenses
    createDestination_clientToken,
    createDestination_state,
    createDestination_additionalFixedProperties,
    createDestination_name,
    createDestination_site,

    -- * Destructuring the Response
    CreateDestinationResponse (..),
    newCreateDestinationResponse,

    -- * Response Lenses
    createDestinationResponse_httpStatus,
    createDestinationResponse_arn,
    createDestinationResponse_id,
    createDestinationResponse_createdAt,
    createDestinationResponse_updatedAt,
    createDestinationResponse_state,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTRoboRunner.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDestination' smart constructor.
data CreateDestination = CreateDestination'
  { clientToken :: Prelude.Maybe Prelude.Text,
    -- | The state of the destination. Default used if not specified.
    state :: Prelude.Maybe DestinationState,
    additionalFixedProperties :: Prelude.Maybe Prelude.Text,
    name :: Prelude.Text,
    site :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createDestination_clientToken' - Undocumented member.
--
-- 'state', 'createDestination_state' - The state of the destination. Default used if not specified.
--
-- 'additionalFixedProperties', 'createDestination_additionalFixedProperties' - Undocumented member.
--
-- 'name', 'createDestination_name' - Undocumented member.
--
-- 'site', 'createDestination_site' - Undocumented member.
newCreateDestination ::
  -- | 'name'
  Prelude.Text ->
  -- | 'site'
  Prelude.Text ->
  CreateDestination
newCreateDestination pName_ pSite_ =
  CreateDestination'
    { clientToken = Prelude.Nothing,
      state = Prelude.Nothing,
      additionalFixedProperties = Prelude.Nothing,
      name = pName_,
      site = pSite_
    }

-- | Undocumented member.
createDestination_clientToken :: Lens.Lens' CreateDestination (Prelude.Maybe Prelude.Text)
createDestination_clientToken = Lens.lens (\CreateDestination' {clientToken} -> clientToken) (\s@CreateDestination' {} a -> s {clientToken = a} :: CreateDestination)

-- | The state of the destination. Default used if not specified.
createDestination_state :: Lens.Lens' CreateDestination (Prelude.Maybe DestinationState)
createDestination_state = Lens.lens (\CreateDestination' {state} -> state) (\s@CreateDestination' {} a -> s {state = a} :: CreateDestination)

-- | Undocumented member.
createDestination_additionalFixedProperties :: Lens.Lens' CreateDestination (Prelude.Maybe Prelude.Text)
createDestination_additionalFixedProperties = Lens.lens (\CreateDestination' {additionalFixedProperties} -> additionalFixedProperties) (\s@CreateDestination' {} a -> s {additionalFixedProperties = a} :: CreateDestination)

-- | Undocumented member.
createDestination_name :: Lens.Lens' CreateDestination Prelude.Text
createDestination_name = Lens.lens (\CreateDestination' {name} -> name) (\s@CreateDestination' {} a -> s {name = a} :: CreateDestination)

-- | Undocumented member.
createDestination_site :: Lens.Lens' CreateDestination Prelude.Text
createDestination_site = Lens.lens (\CreateDestination' {site} -> site) (\s@CreateDestination' {} a -> s {site = a} :: CreateDestination)

instance Core.AWSRequest CreateDestination where
  type
    AWSResponse CreateDestination =
      CreateDestinationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDestinationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "id")
            Prelude.<*> (x Core..:> "createdAt")
            Prelude.<*> (x Core..:> "updatedAt")
            Prelude.<*> (x Core..:> "state")
      )

instance Prelude.Hashable CreateDestination where
  hashWithSalt _salt CreateDestination' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` additionalFixedProperties
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` site

instance Prelude.NFData CreateDestination where
  rnf CreateDestination' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf additionalFixedProperties
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf site

instance Core.ToHeaders CreateDestination where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateDestination where
  toJSON CreateDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("state" Core..=) Prelude.<$> state,
            ("additionalFixedProperties" Core..=)
              Prelude.<$> additionalFixedProperties,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("site" Core..= site)
          ]
      )

instance Core.ToPath CreateDestination where
  toPath = Prelude.const "/createDestination"

instance Core.ToQuery CreateDestination where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDestinationResponse' smart constructor.
data CreateDestinationResponse = CreateDestinationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    arn :: Prelude.Text,
    id :: Prelude.Text,
    createdAt :: Core.POSIX,
    updatedAt :: Core.POSIX,
    state :: DestinationState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDestinationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createDestinationResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'createDestinationResponse_arn' - Undocumented member.
--
-- 'id', 'createDestinationResponse_id' - Undocumented member.
--
-- 'createdAt', 'createDestinationResponse_createdAt' - Undocumented member.
--
-- 'updatedAt', 'createDestinationResponse_updatedAt' - Undocumented member.
--
-- 'state', 'createDestinationResponse_state' - Undocumented member.
newCreateDestinationResponse ::
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
  -- | 'state'
  DestinationState ->
  CreateDestinationResponse
newCreateDestinationResponse
  pHttpStatus_
  pArn_
  pId_
  pCreatedAt_
  pUpdatedAt_
  pState_ =
    CreateDestinationResponse'
      { httpStatus =
          pHttpStatus_,
        arn = pArn_,
        id = pId_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        state = pState_
      }

-- | The response's http status code.
createDestinationResponse_httpStatus :: Lens.Lens' CreateDestinationResponse Prelude.Int
createDestinationResponse_httpStatus = Lens.lens (\CreateDestinationResponse' {httpStatus} -> httpStatus) (\s@CreateDestinationResponse' {} a -> s {httpStatus = a} :: CreateDestinationResponse)

-- | Undocumented member.
createDestinationResponse_arn :: Lens.Lens' CreateDestinationResponse Prelude.Text
createDestinationResponse_arn = Lens.lens (\CreateDestinationResponse' {arn} -> arn) (\s@CreateDestinationResponse' {} a -> s {arn = a} :: CreateDestinationResponse)

-- | Undocumented member.
createDestinationResponse_id :: Lens.Lens' CreateDestinationResponse Prelude.Text
createDestinationResponse_id = Lens.lens (\CreateDestinationResponse' {id} -> id) (\s@CreateDestinationResponse' {} a -> s {id = a} :: CreateDestinationResponse)

-- | Undocumented member.
createDestinationResponse_createdAt :: Lens.Lens' CreateDestinationResponse Prelude.UTCTime
createDestinationResponse_createdAt = Lens.lens (\CreateDestinationResponse' {createdAt} -> createdAt) (\s@CreateDestinationResponse' {} a -> s {createdAt = a} :: CreateDestinationResponse) Prelude.. Core._Time

-- | Undocumented member.
createDestinationResponse_updatedAt :: Lens.Lens' CreateDestinationResponse Prelude.UTCTime
createDestinationResponse_updatedAt = Lens.lens (\CreateDestinationResponse' {updatedAt} -> updatedAt) (\s@CreateDestinationResponse' {} a -> s {updatedAt = a} :: CreateDestinationResponse) Prelude.. Core._Time

-- | Undocumented member.
createDestinationResponse_state :: Lens.Lens' CreateDestinationResponse DestinationState
createDestinationResponse_state = Lens.lens (\CreateDestinationResponse' {state} -> state) (\s@CreateDestinationResponse' {} a -> s {state = a} :: CreateDestinationResponse)

instance Prelude.NFData CreateDestinationResponse where
  rnf CreateDestinationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf state
