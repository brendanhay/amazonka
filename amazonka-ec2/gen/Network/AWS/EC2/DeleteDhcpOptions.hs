{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.DeleteDhcpOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set
-- of DHCP options before you can delete it. You can disassociate the set
-- of DHCP options by associating either a new set of options or the
-- default set of options with the VPC.
module Network.AWS.EC2.DeleteDhcpOptions
  ( -- * Creating a Request
    DeleteDhcpOptions (..),
    newDeleteDhcpOptions,

    -- * Request Lenses
    deleteDhcpOptions_dryRun,
    deleteDhcpOptions_dhcpOptionsId,

    -- * Destructuring the Response
    DeleteDhcpOptionsResponse (..),
    newDeleteDhcpOptionsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDhcpOptions' smart constructor.
data DeleteDhcpOptions = DeleteDhcpOptions'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the DHCP options set.
    dhcpOptionsId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDhcpOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteDhcpOptions_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'dhcpOptionsId', 'deleteDhcpOptions_dhcpOptionsId' - The ID of the DHCP options set.
newDeleteDhcpOptions ::
  -- | 'dhcpOptionsId'
  Prelude.Text ->
  DeleteDhcpOptions
newDeleteDhcpOptions pDhcpOptionsId_ =
  DeleteDhcpOptions'
    { dryRun = Prelude.Nothing,
      dhcpOptionsId = pDhcpOptionsId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteDhcpOptions_dryRun :: Lens.Lens' DeleteDhcpOptions (Prelude.Maybe Prelude.Bool)
deleteDhcpOptions_dryRun = Lens.lens (\DeleteDhcpOptions' {dryRun} -> dryRun) (\s@DeleteDhcpOptions' {} a -> s {dryRun = a} :: DeleteDhcpOptions)

-- | The ID of the DHCP options set.
deleteDhcpOptions_dhcpOptionsId :: Lens.Lens' DeleteDhcpOptions Prelude.Text
deleteDhcpOptions_dhcpOptionsId = Lens.lens (\DeleteDhcpOptions' {dhcpOptionsId} -> dhcpOptionsId) (\s@DeleteDhcpOptions' {} a -> s {dhcpOptionsId = a} :: DeleteDhcpOptions)

instance Prelude.AWSRequest DeleteDhcpOptions where
  type Rs DeleteDhcpOptions = DeleteDhcpOptionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DeleteDhcpOptionsResponse'

instance Prelude.Hashable DeleteDhcpOptions

instance Prelude.NFData DeleteDhcpOptions

instance Prelude.ToHeaders DeleteDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteDhcpOptions where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteDhcpOptions where
  toQuery DeleteDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteDhcpOptions" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        "DhcpOptionsId" Prelude.=: dhcpOptionsId
      ]

-- | /See:/ 'newDeleteDhcpOptionsResponse' smart constructor.
data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDhcpOptionsResponse ::
  DeleteDhcpOptionsResponse
newDeleteDhcpOptionsResponse =
  DeleteDhcpOptionsResponse'

instance Prelude.NFData DeleteDhcpOptionsResponse
