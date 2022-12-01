{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.StorageGateway.Types.SMBLocalGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.SMBLocalGroups where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of Active Directory users and groups that have special
-- permissions for SMB file shares on the gateway.
--
-- /See:/ 'newSMBLocalGroups' smart constructor.
data SMBLocalGroups = SMBLocalGroups'
  { -- | A list of Active Directory users and groups that have local Gateway
    -- Admin permissions. Acceptable formats include: @DOMAIN\\User1@, @user1@,
    -- @DOMAIN\\group1@, and @group1@.
    --
    -- Gateway Admins can use the Shared Folders Microsoft Management Console
    -- snap-in to force-close files that are open and locked.
    gatewayAdmins :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMBLocalGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayAdmins', 'sMBLocalGroups_gatewayAdmins' - A list of Active Directory users and groups that have local Gateway
-- Admin permissions. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @DOMAIN\\group1@, and @group1@.
--
-- Gateway Admins can use the Shared Folders Microsoft Management Console
-- snap-in to force-close files that are open and locked.
newSMBLocalGroups ::
  SMBLocalGroups
newSMBLocalGroups =
  SMBLocalGroups' {gatewayAdmins = Prelude.Nothing}

-- | A list of Active Directory users and groups that have local Gateway
-- Admin permissions. Acceptable formats include: @DOMAIN\\User1@, @user1@,
-- @DOMAIN\\group1@, and @group1@.
--
-- Gateway Admins can use the Shared Folders Microsoft Management Console
-- snap-in to force-close files that are open and locked.
sMBLocalGroups_gatewayAdmins :: Lens.Lens' SMBLocalGroups (Prelude.Maybe [Prelude.Text])
sMBLocalGroups_gatewayAdmins = Lens.lens (\SMBLocalGroups' {gatewayAdmins} -> gatewayAdmins) (\s@SMBLocalGroups' {} a -> s {gatewayAdmins = a} :: SMBLocalGroups) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON SMBLocalGroups where
  parseJSON =
    Core.withObject
      "SMBLocalGroups"
      ( \x ->
          SMBLocalGroups'
            Prelude.<$> (x Core..:? "GatewayAdmins" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable SMBLocalGroups where
  hashWithSalt _salt SMBLocalGroups' {..} =
    _salt `Prelude.hashWithSalt` gatewayAdmins

instance Prelude.NFData SMBLocalGroups where
  rnf SMBLocalGroups' {..} = Prelude.rnf gatewayAdmins

instance Core.ToJSON SMBLocalGroups where
  toJSON SMBLocalGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GatewayAdmins" Core..=)
              Prelude.<$> gatewayAdmins
          ]
      )
