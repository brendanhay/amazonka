{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoT.Types.AuthResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthResult where

import Network.AWS.IoT.Types.Allowed
import Network.AWS.IoT.Types.AuthDecision
import Network.AWS.IoT.Types.AuthInfo
import Network.AWS.IoT.Types.Denied
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The authorizer result.
--
-- /See:/ 'newAuthResult' smart constructor.
data AuthResult = AuthResult'
  { -- | Authorization information.
    authInfo :: Prelude.Maybe AuthInfo,
    -- | The policies and statements that allowed the specified action.
    allowed :: Prelude.Maybe Allowed,
    -- | The policies and statements that denied the specified action.
    denied :: Prelude.Maybe Denied,
    -- | Contains any missing context values found while evaluating policy.
    missingContextValues :: Prelude.Maybe [Prelude.Text],
    -- | The final authorization decision of this scenario. Multiple statements
    -- are taken into account when determining the authorization decision. An
    -- explicit deny statement can override multiple allow statements.
    authDecision :: Prelude.Maybe AuthDecision
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AuthResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authInfo', 'authResult_authInfo' - Authorization information.
--
-- 'allowed', 'authResult_allowed' - The policies and statements that allowed the specified action.
--
-- 'denied', 'authResult_denied' - The policies and statements that denied the specified action.
--
-- 'missingContextValues', 'authResult_missingContextValues' - Contains any missing context values found while evaluating policy.
--
-- 'authDecision', 'authResult_authDecision' - The final authorization decision of this scenario. Multiple statements
-- are taken into account when determining the authorization decision. An
-- explicit deny statement can override multiple allow statements.
newAuthResult ::
  AuthResult
newAuthResult =
  AuthResult'
    { authInfo = Prelude.Nothing,
      allowed = Prelude.Nothing,
      denied = Prelude.Nothing,
      missingContextValues = Prelude.Nothing,
      authDecision = Prelude.Nothing
    }

-- | Authorization information.
authResult_authInfo :: Lens.Lens' AuthResult (Prelude.Maybe AuthInfo)
authResult_authInfo = Lens.lens (\AuthResult' {authInfo} -> authInfo) (\s@AuthResult' {} a -> s {authInfo = a} :: AuthResult)

-- | The policies and statements that allowed the specified action.
authResult_allowed :: Lens.Lens' AuthResult (Prelude.Maybe Allowed)
authResult_allowed = Lens.lens (\AuthResult' {allowed} -> allowed) (\s@AuthResult' {} a -> s {allowed = a} :: AuthResult)

-- | The policies and statements that denied the specified action.
authResult_denied :: Lens.Lens' AuthResult (Prelude.Maybe Denied)
authResult_denied = Lens.lens (\AuthResult' {denied} -> denied) (\s@AuthResult' {} a -> s {denied = a} :: AuthResult)

-- | Contains any missing context values found while evaluating policy.
authResult_missingContextValues :: Lens.Lens' AuthResult (Prelude.Maybe [Prelude.Text])
authResult_missingContextValues = Lens.lens (\AuthResult' {missingContextValues} -> missingContextValues) (\s@AuthResult' {} a -> s {missingContextValues = a} :: AuthResult) Prelude.. Lens.mapping Prelude._Coerce

-- | The final authorization decision of this scenario. Multiple statements
-- are taken into account when determining the authorization decision. An
-- explicit deny statement can override multiple allow statements.
authResult_authDecision :: Lens.Lens' AuthResult (Prelude.Maybe AuthDecision)
authResult_authDecision = Lens.lens (\AuthResult' {authDecision} -> authDecision) (\s@AuthResult' {} a -> s {authDecision = a} :: AuthResult)

instance Prelude.FromJSON AuthResult where
  parseJSON =
    Prelude.withObject
      "AuthResult"
      ( \x ->
          AuthResult'
            Prelude.<$> (x Prelude..:? "authInfo")
            Prelude.<*> (x Prelude..:? "allowed")
            Prelude.<*> (x Prelude..:? "denied")
            Prelude.<*> ( x Prelude..:? "missingContextValues"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "authDecision")
      )

instance Prelude.Hashable AuthResult

instance Prelude.NFData AuthResult
