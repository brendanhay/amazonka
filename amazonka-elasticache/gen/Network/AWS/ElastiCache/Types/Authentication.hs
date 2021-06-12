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
-- Module      : Network.AWS.ElastiCache.Types.Authentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Authentication where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens

-- | Indicates whether the user requires a password to authenticate.
--
-- /See:/ 'newAuthentication' smart constructor.
data Authentication = Authentication'
  { -- | The number of passwords belonging to the user. The maximum is two.
    passwordCount :: Core.Maybe Core.Int,
    -- | Indicates whether the user requires a password to authenticate.
    type' :: Core.Maybe AuthenticationType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Authentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordCount', 'authentication_passwordCount' - The number of passwords belonging to the user. The maximum is two.
--
-- 'type'', 'authentication_type' - Indicates whether the user requires a password to authenticate.
newAuthentication ::
  Authentication
newAuthentication =
  Authentication'
    { passwordCount = Core.Nothing,
      type' = Core.Nothing
    }

-- | The number of passwords belonging to the user. The maximum is two.
authentication_passwordCount :: Lens.Lens' Authentication (Core.Maybe Core.Int)
authentication_passwordCount = Lens.lens (\Authentication' {passwordCount} -> passwordCount) (\s@Authentication' {} a -> s {passwordCount = a} :: Authentication)

-- | Indicates whether the user requires a password to authenticate.
authentication_type :: Lens.Lens' Authentication (Core.Maybe AuthenticationType)
authentication_type = Lens.lens (\Authentication' {type'} -> type') (\s@Authentication' {} a -> s {type' = a} :: Authentication)

instance Core.FromXML Authentication where
  parseXML x =
    Authentication'
      Core.<$> (x Core..@? "PasswordCount")
      Core.<*> (x Core..@? "Type")

instance Core.Hashable Authentication

instance Core.NFData Authentication
