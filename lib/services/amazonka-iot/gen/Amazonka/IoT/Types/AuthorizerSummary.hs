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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.AuthorizerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The authorizer summary.
--
-- /See:/ 'newAuthorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { -- | The authorizer ARN.
    authorizerArn :: Prelude.Maybe Prelude.Text,
    -- | The authorizer name.
    authorizerName :: Prelude.Maybe Prelude.Text
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
-- 'authorizerArn', 'authorizerSummary_authorizerArn' - The authorizer ARN.
--
-- 'authorizerName', 'authorizerSummary_authorizerName' - The authorizer name.
newAuthorizerSummary ::
  AuthorizerSummary
newAuthorizerSummary =
  AuthorizerSummary'
    { authorizerArn = Prelude.Nothing,
      authorizerName = Prelude.Nothing
    }

-- | The authorizer ARN.
authorizerSummary_authorizerArn :: Lens.Lens' AuthorizerSummary (Prelude.Maybe Prelude.Text)
authorizerSummary_authorizerArn = Lens.lens (\AuthorizerSummary' {authorizerArn} -> authorizerArn) (\s@AuthorizerSummary' {} a -> s {authorizerArn = a} :: AuthorizerSummary)

-- | The authorizer name.
authorizerSummary_authorizerName :: Lens.Lens' AuthorizerSummary (Prelude.Maybe Prelude.Text)
authorizerSummary_authorizerName = Lens.lens (\AuthorizerSummary' {authorizerName} -> authorizerName) (\s@AuthorizerSummary' {} a -> s {authorizerName = a} :: AuthorizerSummary)

instance Data.FromJSON AuthorizerSummary where
  parseJSON =
    Data.withObject
      "AuthorizerSummary"
      ( \x ->
          AuthorizerSummary'
            Prelude.<$> (x Data..:? "authorizerArn")
            Prelude.<*> (x Data..:? "authorizerName")
      )

instance Prelude.Hashable AuthorizerSummary where
  hashWithSalt _salt AuthorizerSummary' {..} =
    _salt
      `Prelude.hashWithSalt` authorizerArn
      `Prelude.hashWithSalt` authorizerName

instance Prelude.NFData AuthorizerSummary where
  rnf AuthorizerSummary' {..} =
    Prelude.rnf authorizerArn
      `Prelude.seq` Prelude.rnf authorizerName
