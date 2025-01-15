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
-- Module      : Amazonka.ManagedBlockChain.Types.MemberFabricAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.MemberFabricAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Attributes of Hyperledger Fabric for a member in a Managed Blockchain
-- network using the Hyperledger Fabric framework.
--
-- /See:/ 'newMemberFabricAttributes' smart constructor.
data MemberFabricAttributes = MemberFabricAttributes'
  { -- | The user name for the initial administrator user for the member.
    adminUsername :: Prelude.Maybe Prelude.Text,
    -- | The endpoint used to access the member\'s certificate authority.
    caEndpoint :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberFabricAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adminUsername', 'memberFabricAttributes_adminUsername' - The user name for the initial administrator user for the member.
--
-- 'caEndpoint', 'memberFabricAttributes_caEndpoint' - The endpoint used to access the member\'s certificate authority.
newMemberFabricAttributes ::
  MemberFabricAttributes
newMemberFabricAttributes =
  MemberFabricAttributes'
    { adminUsername =
        Prelude.Nothing,
      caEndpoint = Prelude.Nothing
    }

-- | The user name for the initial administrator user for the member.
memberFabricAttributes_adminUsername :: Lens.Lens' MemberFabricAttributes (Prelude.Maybe Prelude.Text)
memberFabricAttributes_adminUsername = Lens.lens (\MemberFabricAttributes' {adminUsername} -> adminUsername) (\s@MemberFabricAttributes' {} a -> s {adminUsername = a} :: MemberFabricAttributes)

-- | The endpoint used to access the member\'s certificate authority.
memberFabricAttributes_caEndpoint :: Lens.Lens' MemberFabricAttributes (Prelude.Maybe Prelude.Text)
memberFabricAttributes_caEndpoint = Lens.lens (\MemberFabricAttributes' {caEndpoint} -> caEndpoint) (\s@MemberFabricAttributes' {} a -> s {caEndpoint = a} :: MemberFabricAttributes)

instance Data.FromJSON MemberFabricAttributes where
  parseJSON =
    Data.withObject
      "MemberFabricAttributes"
      ( \x ->
          MemberFabricAttributes'
            Prelude.<$> (x Data..:? "AdminUsername")
            Prelude.<*> (x Data..:? "CaEndpoint")
      )

instance Prelude.Hashable MemberFabricAttributes where
  hashWithSalt _salt MemberFabricAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` adminUsername
      `Prelude.hashWithSalt` caEndpoint

instance Prelude.NFData MemberFabricAttributes where
  rnf MemberFabricAttributes' {..} =
    Prelude.rnf adminUsername `Prelude.seq`
      Prelude.rnf caEndpoint
