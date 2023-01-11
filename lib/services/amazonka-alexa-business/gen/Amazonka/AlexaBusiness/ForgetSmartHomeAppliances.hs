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
-- Module      : Amazonka.AlexaBusiness.ForgetSmartHomeAppliances
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets smart home appliances associated to a room.
module Amazonka.AlexaBusiness.ForgetSmartHomeAppliances
  ( -- * Creating a Request
    ForgetSmartHomeAppliances (..),
    newForgetSmartHomeAppliances,

    -- * Request Lenses
    forgetSmartHomeAppliances_roomArn,

    -- * Destructuring the Response
    ForgetSmartHomeAppliancesResponse (..),
    newForgetSmartHomeAppliancesResponse,

    -- * Response Lenses
    forgetSmartHomeAppliancesResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newForgetSmartHomeAppliances' smart constructor.
data ForgetSmartHomeAppliances = ForgetSmartHomeAppliances'
  { -- | The room that the appliances are associated with.
    roomArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgetSmartHomeAppliances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'roomArn', 'forgetSmartHomeAppliances_roomArn' - The room that the appliances are associated with.
newForgetSmartHomeAppliances ::
  -- | 'roomArn'
  Prelude.Text ->
  ForgetSmartHomeAppliances
newForgetSmartHomeAppliances pRoomArn_ =
  ForgetSmartHomeAppliances' {roomArn = pRoomArn_}

-- | The room that the appliances are associated with.
forgetSmartHomeAppliances_roomArn :: Lens.Lens' ForgetSmartHomeAppliances Prelude.Text
forgetSmartHomeAppliances_roomArn = Lens.lens (\ForgetSmartHomeAppliances' {roomArn} -> roomArn) (\s@ForgetSmartHomeAppliances' {} a -> s {roomArn = a} :: ForgetSmartHomeAppliances)

instance Core.AWSRequest ForgetSmartHomeAppliances where
  type
    AWSResponse ForgetSmartHomeAppliances =
      ForgetSmartHomeAppliancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          ForgetSmartHomeAppliancesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ForgetSmartHomeAppliances where
  hashWithSalt _salt ForgetSmartHomeAppliances' {..} =
    _salt `Prelude.hashWithSalt` roomArn

instance Prelude.NFData ForgetSmartHomeAppliances where
  rnf ForgetSmartHomeAppliances' {..} =
    Prelude.rnf roomArn

instance Data.ToHeaders ForgetSmartHomeAppliances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AlexaForBusiness.ForgetSmartHomeAppliances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ForgetSmartHomeAppliances where
  toJSON ForgetSmartHomeAppliances' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RoomArn" Data..= roomArn)]
      )

instance Data.ToPath ForgetSmartHomeAppliances where
  toPath = Prelude.const "/"

instance Data.ToQuery ForgetSmartHomeAppliances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newForgetSmartHomeAppliancesResponse' smart constructor.
data ForgetSmartHomeAppliancesResponse = ForgetSmartHomeAppliancesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForgetSmartHomeAppliancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'forgetSmartHomeAppliancesResponse_httpStatus' - The response's http status code.
newForgetSmartHomeAppliancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ForgetSmartHomeAppliancesResponse
newForgetSmartHomeAppliancesResponse pHttpStatus_ =
  ForgetSmartHomeAppliancesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
forgetSmartHomeAppliancesResponse_httpStatus :: Lens.Lens' ForgetSmartHomeAppliancesResponse Prelude.Int
forgetSmartHomeAppliancesResponse_httpStatus = Lens.lens (\ForgetSmartHomeAppliancesResponse' {httpStatus} -> httpStatus) (\s@ForgetSmartHomeAppliancesResponse' {} a -> s {httpStatus = a} :: ForgetSmartHomeAppliancesResponse)

instance
  Prelude.NFData
    ForgetSmartHomeAppliancesResponse
  where
  rnf ForgetSmartHomeAppliancesResponse' {..} =
    Prelude.rnf httpStatus
