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
-- Module      : Network.AWS.IoT.Types.AuthorizerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The authorizer summary.
--
-- /See:/ 'newAuthorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { -- | The authorizer ARN.
    authorizerArn :: Core.Maybe Core.Text,
    -- | The authorizer name.
    authorizerName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { authorizerArn = Core.Nothing,
      authorizerName = Core.Nothing
    }

-- | The authorizer ARN.
authorizerSummary_authorizerArn :: Lens.Lens' AuthorizerSummary (Core.Maybe Core.Text)
authorizerSummary_authorizerArn = Lens.lens (\AuthorizerSummary' {authorizerArn} -> authorizerArn) (\s@AuthorizerSummary' {} a -> s {authorizerArn = a} :: AuthorizerSummary)

-- | The authorizer name.
authorizerSummary_authorizerName :: Lens.Lens' AuthorizerSummary (Core.Maybe Core.Text)
authorizerSummary_authorizerName = Lens.lens (\AuthorizerSummary' {authorizerName} -> authorizerName) (\s@AuthorizerSummary' {} a -> s {authorizerName = a} :: AuthorizerSummary)

instance Core.FromJSON AuthorizerSummary where
  parseJSON =
    Core.withObject
      "AuthorizerSummary"
      ( \x ->
          AuthorizerSummary'
            Core.<$> (x Core..:? "authorizerArn")
            Core.<*> (x Core..:? "authorizerName")
      )

instance Core.Hashable AuthorizerSummary

instance Core.NFData AuthorizerSummary
