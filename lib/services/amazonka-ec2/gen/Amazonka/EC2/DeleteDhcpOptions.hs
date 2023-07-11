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
-- Module      : Amazonka.EC2.DeleteDhcpOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set
-- of DHCP options before you can delete it. You can disassociate the set
-- of DHCP options by associating either a new set of options or the
-- default set of options with the VPC.
module Amazonka.EC2.DeleteDhcpOptions
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteDhcpOptions where
  type
    AWSResponse DeleteDhcpOptions =
      DeleteDhcpOptionsResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull DeleteDhcpOptionsResponse'

instance Prelude.Hashable DeleteDhcpOptions where
  hashWithSalt _salt DeleteDhcpOptions' {..} =
    _salt
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` dhcpOptionsId

instance Prelude.NFData DeleteDhcpOptions where
  rnf DeleteDhcpOptions' {..} =
    Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf dhcpOptionsId

instance Data.ToHeaders DeleteDhcpOptions where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteDhcpOptions where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteDhcpOptions where
  toQuery DeleteDhcpOptions' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteDhcpOptions" :: Prelude.ByteString),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Data.=: dryRun,
        "DhcpOptionsId" Data.=: dhcpOptionsId
      ]

-- | /See:/ 'newDeleteDhcpOptionsResponse' smart constructor.
data DeleteDhcpOptionsResponse = DeleteDhcpOptionsResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDhcpOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDhcpOptionsResponse ::
  DeleteDhcpOptionsResponse
newDeleteDhcpOptionsResponse =
  DeleteDhcpOptionsResponse'

instance Prelude.NFData DeleteDhcpOptionsResponse where
  rnf _ = ()
