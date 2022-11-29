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
-- Module      : Amazonka.IoTWireless.DisassociateWirelessDeviceFromMulticastGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates a wireless device from a multicast group.
module Amazonka.IoTWireless.DisassociateWirelessDeviceFromMulticastGroup
  ( -- * Creating a Request
    DisassociateWirelessDeviceFromMulticastGroup (..),
    newDisassociateWirelessDeviceFromMulticastGroup,

    -- * Request Lenses
    disassociateWirelessDeviceFromMulticastGroup_id,
    disassociateWirelessDeviceFromMulticastGroup_wirelessDeviceId,

    -- * Destructuring the Response
    DisassociateWirelessDeviceFromMulticastGroupResponse (..),
    newDisassociateWirelessDeviceFromMulticastGroupResponse,

    -- * Response Lenses
    disassociateWirelessDeviceFromMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDisassociateWirelessDeviceFromMulticastGroup' smart constructor.
data DisassociateWirelessDeviceFromMulticastGroup = DisassociateWirelessDeviceFromMulticastGroup'
  { id :: Prelude.Text,
    wirelessDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'disassociateWirelessDeviceFromMulticastGroup_id' - Undocumented member.
--
-- 'wirelessDeviceId', 'disassociateWirelessDeviceFromMulticastGroup_wirelessDeviceId' - Undocumented member.
newDisassociateWirelessDeviceFromMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'wirelessDeviceId'
  Prelude.Text ->
  DisassociateWirelessDeviceFromMulticastGroup
newDisassociateWirelessDeviceFromMulticastGroup
  pId_
  pWirelessDeviceId_ =
    DisassociateWirelessDeviceFromMulticastGroup'
      { id =
          pId_,
        wirelessDeviceId =
          pWirelessDeviceId_
      }

-- | Undocumented member.
disassociateWirelessDeviceFromMulticastGroup_id :: Lens.Lens' DisassociateWirelessDeviceFromMulticastGroup Prelude.Text
disassociateWirelessDeviceFromMulticastGroup_id = Lens.lens (\DisassociateWirelessDeviceFromMulticastGroup' {id} -> id) (\s@DisassociateWirelessDeviceFromMulticastGroup' {} a -> s {id = a} :: DisassociateWirelessDeviceFromMulticastGroup)

-- | Undocumented member.
disassociateWirelessDeviceFromMulticastGroup_wirelessDeviceId :: Lens.Lens' DisassociateWirelessDeviceFromMulticastGroup Prelude.Text
disassociateWirelessDeviceFromMulticastGroup_wirelessDeviceId = Lens.lens (\DisassociateWirelessDeviceFromMulticastGroup' {wirelessDeviceId} -> wirelessDeviceId) (\s@DisassociateWirelessDeviceFromMulticastGroup' {} a -> s {wirelessDeviceId = a} :: DisassociateWirelessDeviceFromMulticastGroup)

instance
  Core.AWSRequest
    DisassociateWirelessDeviceFromMulticastGroup
  where
  type
    AWSResponse
      DisassociateWirelessDeviceFromMulticastGroup =
      DisassociateWirelessDeviceFromMulticastGroupResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DisassociateWirelessDeviceFromMulticastGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateWirelessDeviceFromMulticastGroup
  where
  hashWithSalt
    _salt
    DisassociateWirelessDeviceFromMulticastGroup' {..} =
      _salt `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` wirelessDeviceId

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromMulticastGroup
  where
  rnf DisassociateWirelessDeviceFromMulticastGroup' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf wirelessDeviceId

instance
  Core.ToHeaders
    DisassociateWirelessDeviceFromMulticastGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToPath
    DisassociateWirelessDeviceFromMulticastGroup
  where
  toPath
    DisassociateWirelessDeviceFromMulticastGroup' {..} =
      Prelude.mconcat
        [ "/multicast-groups/",
          Core.toBS id,
          "/wireless-devices/",
          Core.toBS wirelessDeviceId
        ]

instance
  Core.ToQuery
    DisassociateWirelessDeviceFromMulticastGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateWirelessDeviceFromMulticastGroupResponse' smart constructor.
data DisassociateWirelessDeviceFromMulticastGroupResponse = DisassociateWirelessDeviceFromMulticastGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateWirelessDeviceFromMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disassociateWirelessDeviceFromMulticastGroupResponse_httpStatus' - The response's http status code.
newDisassociateWirelessDeviceFromMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateWirelessDeviceFromMulticastGroupResponse
newDisassociateWirelessDeviceFromMulticastGroupResponse
  pHttpStatus_ =
    DisassociateWirelessDeviceFromMulticastGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
disassociateWirelessDeviceFromMulticastGroupResponse_httpStatus :: Lens.Lens' DisassociateWirelessDeviceFromMulticastGroupResponse Prelude.Int
disassociateWirelessDeviceFromMulticastGroupResponse_httpStatus = Lens.lens (\DisassociateWirelessDeviceFromMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@DisassociateWirelessDeviceFromMulticastGroupResponse' {} a -> s {httpStatus = a} :: DisassociateWirelessDeviceFromMulticastGroupResponse)

instance
  Prelude.NFData
    DisassociateWirelessDeviceFromMulticastGroupResponse
  where
  rnf
    DisassociateWirelessDeviceFromMulticastGroupResponse' {..} =
      Prelude.rnf httpStatus
