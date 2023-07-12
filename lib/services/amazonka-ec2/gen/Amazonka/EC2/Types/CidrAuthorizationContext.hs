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
-- Module      : Amazonka.EC2.Types.CidrAuthorizationContext
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CidrAuthorizationContext where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Provides authorization for Amazon to bring a specific IP address range
-- to a specific Amazon Web Services account using bring your own IP
-- addresses (BYOIP). For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-byoip.html#prepare-for-byoip Configuring your BYOIP address range>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ 'newCidrAuthorizationContext' smart constructor.
data CidrAuthorizationContext = CidrAuthorizationContext'
  { -- | The plain-text authorization message for the prefix and account.
    message :: Prelude.Text,
    -- | The signed authorization message for the prefix and account.
    signature :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CidrAuthorizationContext' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'cidrAuthorizationContext_message' - The plain-text authorization message for the prefix and account.
--
-- 'signature', 'cidrAuthorizationContext_signature' - The signed authorization message for the prefix and account.
newCidrAuthorizationContext ::
  -- | 'message'
  Prelude.Text ->
  -- | 'signature'
  Prelude.Text ->
  CidrAuthorizationContext
newCidrAuthorizationContext pMessage_ pSignature_ =
  CidrAuthorizationContext'
    { message = pMessage_,
      signature = pSignature_
    }

-- | The plain-text authorization message for the prefix and account.
cidrAuthorizationContext_message :: Lens.Lens' CidrAuthorizationContext Prelude.Text
cidrAuthorizationContext_message = Lens.lens (\CidrAuthorizationContext' {message} -> message) (\s@CidrAuthorizationContext' {} a -> s {message = a} :: CidrAuthorizationContext)

-- | The signed authorization message for the prefix and account.
cidrAuthorizationContext_signature :: Lens.Lens' CidrAuthorizationContext Prelude.Text
cidrAuthorizationContext_signature = Lens.lens (\CidrAuthorizationContext' {signature} -> signature) (\s@CidrAuthorizationContext' {} a -> s {signature = a} :: CidrAuthorizationContext)

instance Prelude.Hashable CidrAuthorizationContext where
  hashWithSalt _salt CidrAuthorizationContext' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` signature

instance Prelude.NFData CidrAuthorizationContext where
  rnf CidrAuthorizationContext' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf signature

instance Data.ToQuery CidrAuthorizationContext where
  toQuery CidrAuthorizationContext' {..} =
    Prelude.mconcat
      [ "Message" Data.=: message,
        "Signature" Data.=: signature
      ]
