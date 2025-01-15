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
-- Module      : Amazonka.EC2.Types.IpamCidrAuthorizationContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.IpamCidrAuthorizationContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | A signed document that proves that you are authorized to bring the
-- specified IP address range to Amazon using BYOIP.
--
-- /See:/ 'newIpamCidrAuthorizationContext' smart constructor.
data IpamCidrAuthorizationContext = IpamCidrAuthorizationContext'
  { -- | The plain-text authorization message for the prefix and account.
    message :: Prelude.Maybe Prelude.Text,
    -- | The signed authorization message for the prefix and account.
    signature :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpamCidrAuthorizationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'ipamCidrAuthorizationContext_message' - The plain-text authorization message for the prefix and account.
--
-- 'signature', 'ipamCidrAuthorizationContext_signature' - The signed authorization message for the prefix and account.
newIpamCidrAuthorizationContext ::
  IpamCidrAuthorizationContext
newIpamCidrAuthorizationContext =
  IpamCidrAuthorizationContext'
    { message =
        Prelude.Nothing,
      signature = Prelude.Nothing
    }

-- | The plain-text authorization message for the prefix and account.
ipamCidrAuthorizationContext_message :: Lens.Lens' IpamCidrAuthorizationContext (Prelude.Maybe Prelude.Text)
ipamCidrAuthorizationContext_message = Lens.lens (\IpamCidrAuthorizationContext' {message} -> message) (\s@IpamCidrAuthorizationContext' {} a -> s {message = a} :: IpamCidrAuthorizationContext)

-- | The signed authorization message for the prefix and account.
ipamCidrAuthorizationContext_signature :: Lens.Lens' IpamCidrAuthorizationContext (Prelude.Maybe Prelude.Text)
ipamCidrAuthorizationContext_signature = Lens.lens (\IpamCidrAuthorizationContext' {signature} -> signature) (\s@IpamCidrAuthorizationContext' {} a -> s {signature = a} :: IpamCidrAuthorizationContext)

instance
  Prelude.Hashable
    IpamCidrAuthorizationContext
  where
  hashWithSalt _salt IpamCidrAuthorizationContext' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` signature

instance Prelude.NFData IpamCidrAuthorizationContext where
  rnf IpamCidrAuthorizationContext' {..} =
    Prelude.rnf message `Prelude.seq`
      Prelude.rnf signature

instance Data.ToQuery IpamCidrAuthorizationContext where
  toQuery IpamCidrAuthorizationContext' {..} =
    Prelude.mconcat
      [ "Message" Data.=: message,
        "Signature" Data.=: signature
      ]
