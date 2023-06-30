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
-- Module      : Amazonka.IoT.Types.AuthResult
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Allowed
import Amazonka.IoT.Types.AuthDecision
import Amazonka.IoT.Types.AuthInfo
import Amazonka.IoT.Types.Denied
import qualified Amazonka.Prelude as Prelude

-- | The authorizer result.
--
-- /See:/ 'newAuthResult' smart constructor.
data AuthResult = AuthResult'
  { -- | The policies and statements that allowed the specified action.
    allowed :: Prelude.Maybe Allowed,
    -- | The final authorization decision of this scenario. Multiple statements
    -- are taken into account when determining the authorization decision. An
    -- explicit deny statement can override multiple allow statements.
    authDecision :: Prelude.Maybe AuthDecision,
    -- | Authorization information.
    authInfo :: Prelude.Maybe AuthInfo,
    -- | The policies and statements that denied the specified action.
    denied :: Prelude.Maybe Denied,
    -- | Contains any missing context values found while evaluating policy.
    missingContextValues :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allowed', 'authResult_allowed' - The policies and statements that allowed the specified action.
--
-- 'authDecision', 'authResult_authDecision' - The final authorization decision of this scenario. Multiple statements
-- are taken into account when determining the authorization decision. An
-- explicit deny statement can override multiple allow statements.
--
-- 'authInfo', 'authResult_authInfo' - Authorization information.
--
-- 'denied', 'authResult_denied' - The policies and statements that denied the specified action.
--
-- 'missingContextValues', 'authResult_missingContextValues' - Contains any missing context values found while evaluating policy.
newAuthResult ::
  AuthResult
newAuthResult =
  AuthResult'
    { allowed = Prelude.Nothing,
      authDecision = Prelude.Nothing,
      authInfo = Prelude.Nothing,
      denied = Prelude.Nothing,
      missingContextValues = Prelude.Nothing
    }

-- | The policies and statements that allowed the specified action.
authResult_allowed :: Lens.Lens' AuthResult (Prelude.Maybe Allowed)
authResult_allowed = Lens.lens (\AuthResult' {allowed} -> allowed) (\s@AuthResult' {} a -> s {allowed = a} :: AuthResult)

-- | The final authorization decision of this scenario. Multiple statements
-- are taken into account when determining the authorization decision. An
-- explicit deny statement can override multiple allow statements.
authResult_authDecision :: Lens.Lens' AuthResult (Prelude.Maybe AuthDecision)
authResult_authDecision = Lens.lens (\AuthResult' {authDecision} -> authDecision) (\s@AuthResult' {} a -> s {authDecision = a} :: AuthResult)

-- | Authorization information.
authResult_authInfo :: Lens.Lens' AuthResult (Prelude.Maybe AuthInfo)
authResult_authInfo = Lens.lens (\AuthResult' {authInfo} -> authInfo) (\s@AuthResult' {} a -> s {authInfo = a} :: AuthResult)

-- | The policies and statements that denied the specified action.
authResult_denied :: Lens.Lens' AuthResult (Prelude.Maybe Denied)
authResult_denied = Lens.lens (\AuthResult' {denied} -> denied) (\s@AuthResult' {} a -> s {denied = a} :: AuthResult)

-- | Contains any missing context values found while evaluating policy.
authResult_missingContextValues :: Lens.Lens' AuthResult (Prelude.Maybe [Prelude.Text])
authResult_missingContextValues = Lens.lens (\AuthResult' {missingContextValues} -> missingContextValues) (\s@AuthResult' {} a -> s {missingContextValues = a} :: AuthResult) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON AuthResult where
  parseJSON =
    Data.withObject
      "AuthResult"
      ( \x ->
          AuthResult'
            Prelude.<$> (x Data..:? "allowed")
            Prelude.<*> (x Data..:? "authDecision")
            Prelude.<*> (x Data..:? "authInfo")
            Prelude.<*> (x Data..:? "denied")
            Prelude.<*> ( x
                            Data..:? "missingContextValues"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AuthResult where
  hashWithSalt _salt AuthResult' {..} =
    _salt
      `Prelude.hashWithSalt` allowed
      `Prelude.hashWithSalt` authDecision
      `Prelude.hashWithSalt` authInfo
      `Prelude.hashWithSalt` denied
      `Prelude.hashWithSalt` missingContextValues

instance Prelude.NFData AuthResult where
  rnf AuthResult' {..} =
    Prelude.rnf allowed
      `Prelude.seq` Prelude.rnf authDecision
      `Prelude.seq` Prelude.rnf authInfo
      `Prelude.seq` Prelude.rnf denied
      `Prelude.seq` Prelude.rnf missingContextValues
