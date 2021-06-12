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
-- Module      : Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets smart home appliances associated to a room.
module Network.AWS.AlexaBusiness.ForgetSmartHomeAppliances
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

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newForgetSmartHomeAppliances' smart constructor.
data ForgetSmartHomeAppliances = ForgetSmartHomeAppliances'
  { -- | The room that the appliances are associated with.
    roomArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ForgetSmartHomeAppliances
newForgetSmartHomeAppliances pRoomArn_ =
  ForgetSmartHomeAppliances' {roomArn = pRoomArn_}

-- | The room that the appliances are associated with.
forgetSmartHomeAppliances_roomArn :: Lens.Lens' ForgetSmartHomeAppliances Core.Text
forgetSmartHomeAppliances_roomArn = Lens.lens (\ForgetSmartHomeAppliances' {roomArn} -> roomArn) (\s@ForgetSmartHomeAppliances' {} a -> s {roomArn = a} :: ForgetSmartHomeAppliances)

instance Core.AWSRequest ForgetSmartHomeAppliances where
  type
    AWSResponse ForgetSmartHomeAppliances =
      ForgetSmartHomeAppliancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          ForgetSmartHomeAppliancesResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ForgetSmartHomeAppliances

instance Core.NFData ForgetSmartHomeAppliances

instance Core.ToHeaders ForgetSmartHomeAppliances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.ForgetSmartHomeAppliances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ForgetSmartHomeAppliances where
  toJSON ForgetSmartHomeAppliances' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("RoomArn" Core..= roomArn)]
      )

instance Core.ToPath ForgetSmartHomeAppliances where
  toPath = Core.const "/"

instance Core.ToQuery ForgetSmartHomeAppliances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newForgetSmartHomeAppliancesResponse' smart constructor.
data ForgetSmartHomeAppliancesResponse = ForgetSmartHomeAppliancesResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ForgetSmartHomeAppliancesResponse
newForgetSmartHomeAppliancesResponse pHttpStatus_ =
  ForgetSmartHomeAppliancesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
forgetSmartHomeAppliancesResponse_httpStatus :: Lens.Lens' ForgetSmartHomeAppliancesResponse Core.Int
forgetSmartHomeAppliancesResponse_httpStatus = Lens.lens (\ForgetSmartHomeAppliancesResponse' {httpStatus} -> httpStatus) (\s@ForgetSmartHomeAppliancesResponse' {} a -> s {httpStatus = a} :: ForgetSmartHomeAppliancesResponse)

instance
  Core.NFData
    ForgetSmartHomeAppliancesResponse
