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
-- Module      : Amazonka.IoTWireless.AssociateWirelessDeviceWithMulticastGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a wireless device with a multicast group.
module Amazonka.IoTWireless.AssociateWirelessDeviceWithMulticastGroup
  ( -- * Creating a Request
    AssociateWirelessDeviceWithMulticastGroup (..),
    newAssociateWirelessDeviceWithMulticastGroup,

    -- * Request Lenses
    associateWirelessDeviceWithMulticastGroup_id,
    associateWirelessDeviceWithMulticastGroup_wirelessDeviceId,

    -- * Destructuring the Response
    AssociateWirelessDeviceWithMulticastGroupResponse (..),
    newAssociateWirelessDeviceWithMulticastGroupResponse,

    -- * Response Lenses
    associateWirelessDeviceWithMulticastGroupResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateWirelessDeviceWithMulticastGroup' smart constructor.
data AssociateWirelessDeviceWithMulticastGroup = AssociateWirelessDeviceWithMulticastGroup'
  { id :: Prelude.Text,
    wirelessDeviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithMulticastGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'associateWirelessDeviceWithMulticastGroup_id' - Undocumented member.
--
-- 'wirelessDeviceId', 'associateWirelessDeviceWithMulticastGroup_wirelessDeviceId' - Undocumented member.
newAssociateWirelessDeviceWithMulticastGroup ::
  -- | 'id'
  Prelude.Text ->
  -- | 'wirelessDeviceId'
  Prelude.Text ->
  AssociateWirelessDeviceWithMulticastGroup
newAssociateWirelessDeviceWithMulticastGroup
  pId_
  pWirelessDeviceId_ =
    AssociateWirelessDeviceWithMulticastGroup'
      { id =
          pId_,
        wirelessDeviceId =
          pWirelessDeviceId_
      }

-- | Undocumented member.
associateWirelessDeviceWithMulticastGroup_id :: Lens.Lens' AssociateWirelessDeviceWithMulticastGroup Prelude.Text
associateWirelessDeviceWithMulticastGroup_id = Lens.lens (\AssociateWirelessDeviceWithMulticastGroup' {id} -> id) (\s@AssociateWirelessDeviceWithMulticastGroup' {} a -> s {id = a} :: AssociateWirelessDeviceWithMulticastGroup)

-- | Undocumented member.
associateWirelessDeviceWithMulticastGroup_wirelessDeviceId :: Lens.Lens' AssociateWirelessDeviceWithMulticastGroup Prelude.Text
associateWirelessDeviceWithMulticastGroup_wirelessDeviceId = Lens.lens (\AssociateWirelessDeviceWithMulticastGroup' {wirelessDeviceId} -> wirelessDeviceId) (\s@AssociateWirelessDeviceWithMulticastGroup' {} a -> s {wirelessDeviceId = a} :: AssociateWirelessDeviceWithMulticastGroup)

instance
  Core.AWSRequest
    AssociateWirelessDeviceWithMulticastGroup
  where
  type
    AWSResponse
      AssociateWirelessDeviceWithMulticastGroup =
      AssociateWirelessDeviceWithMulticastGroupResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          AssociateWirelessDeviceWithMulticastGroupResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AssociateWirelessDeviceWithMulticastGroup
  where
  hashWithSalt
    _salt
    AssociateWirelessDeviceWithMulticastGroup' {..} =
      _salt
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` wirelessDeviceId

instance
  Prelude.NFData
    AssociateWirelessDeviceWithMulticastGroup
  where
  rnf AssociateWirelessDeviceWithMulticastGroup' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf wirelessDeviceId

instance
  Data.ToHeaders
    AssociateWirelessDeviceWithMulticastGroup
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    AssociateWirelessDeviceWithMulticastGroup
  where
  toJSON AssociateWirelessDeviceWithMulticastGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("WirelessDeviceId" Data..= wirelessDeviceId)
          ]
      )

instance
  Data.ToPath
    AssociateWirelessDeviceWithMulticastGroup
  where
  toPath AssociateWirelessDeviceWithMulticastGroup' {..} =
    Prelude.mconcat
      [ "/multicast-groups/",
        Data.toBS id,
        "/wireless-device"
      ]

instance
  Data.ToQuery
    AssociateWirelessDeviceWithMulticastGroup
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateWirelessDeviceWithMulticastGroupResponse' smart constructor.
data AssociateWirelessDeviceWithMulticastGroupResponse = AssociateWirelessDeviceWithMulticastGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateWirelessDeviceWithMulticastGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'associateWirelessDeviceWithMulticastGroupResponse_httpStatus' - The response's http status code.
newAssociateWirelessDeviceWithMulticastGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateWirelessDeviceWithMulticastGroupResponse
newAssociateWirelessDeviceWithMulticastGroupResponse
  pHttpStatus_ =
    AssociateWirelessDeviceWithMulticastGroupResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
associateWirelessDeviceWithMulticastGroupResponse_httpStatus :: Lens.Lens' AssociateWirelessDeviceWithMulticastGroupResponse Prelude.Int
associateWirelessDeviceWithMulticastGroupResponse_httpStatus = Lens.lens (\AssociateWirelessDeviceWithMulticastGroupResponse' {httpStatus} -> httpStatus) (\s@AssociateWirelessDeviceWithMulticastGroupResponse' {} a -> s {httpStatus = a} :: AssociateWirelessDeviceWithMulticastGroupResponse)

instance
  Prelude.NFData
    AssociateWirelessDeviceWithMulticastGroupResponse
  where
  rnf
    AssociateWirelessDeviceWithMulticastGroupResponse' {..} =
      Prelude.rnf httpStatus
