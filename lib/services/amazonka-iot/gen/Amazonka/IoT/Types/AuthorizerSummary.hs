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
-- Module      : Amazonka.IoT.Types.AuthorizerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthorizerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The authorizer summary.
--
-- /See:/ 'newAuthorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text,
    -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AuthorizerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerName', 'authorizerSummary_authorizerName' - The authorizer name.
--
-- 'authorizerArn', 'authorizerSummary_authorizerArn' - The authorizer ARN.
newAuthorizerSummary ::
  AuthorizerSummary
newAuthorizerSummary =
  AuthorizerSummary'
    { authorizerName =
        Prelude.Nothing,
      authorizerArn = Prelude.Nothing
    }

-- | The authorizer name.
authorizerSummary_authorizerName :: Lens.Lens' AuthorizerSummary (Prelude.Maybe Prelude.Text)
authorizerSummary_authorizerName = Lens.lens (\AuthorizerSummary' {authorizerName} -> authorizerName) (\s@AuthorizerSummary' {} a -> s {authorizerName = a} :: AuthorizerSummary)

-- | The authorizer ARN.
authorizerSummary_authorizerArn :: Lens.Lens' AuthorizerSummary (Prelude.Maybe Prelude.Text)
authorizerSummary_authorizerArn = Lens.lens (\AuthorizerSummary' {authorizerArn} -> authorizerArn) (\s@AuthorizerSummary' {} a -> s {authorizerArn = a} :: AuthorizerSummary)

instance Core.FromJSON AuthorizerSummary where
  parseJSON =
    Core.withObject
      "AuthorizerSummary"
      ( \x ->
          AuthorizerSummary'
            Prelude.<$> (x Core..:? "authorizerName")
            Prelude.<*> (x Core..:? "authorizerArn")
      )

instance Prelude.Hashable AuthorizerSummary where
  hashWithSalt salt' AuthorizerSummary' {..} =
    salt' `Prelude.hashWithSalt` authorizerArn
      `Prelude.hashWithSalt` authorizerName

instance Prelude.NFData AuthorizerSummary where
  rnf AuthorizerSummary' {..} =
    Prelude.rnf authorizerName
      `Prelude.seq` Prelude.rnf authorizerArn
