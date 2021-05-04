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
-- Module      : Network.AWS.ElastiCache.Types.Authentication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Authentication where

import Network.AWS.ElastiCache.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Indicates whether the user requires a password to authenticate.
--
-- /See:/ 'newAuthentication' smart constructor.
data Authentication = Authentication'
  { -- | The number of passwords belonging to the user. The maximum is two.
    passwordCount :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the user requires a password to authenticate.
    type' :: Prelude.Maybe AuthenticationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { passwordCount = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The number of passwords belonging to the user. The maximum is two.
authentication_passwordCount :: Lens.Lens' Authentication (Prelude.Maybe Prelude.Int)
authentication_passwordCount = Lens.lens (\Authentication' {passwordCount} -> passwordCount) (\s@Authentication' {} a -> s {passwordCount = a} :: Authentication)

-- | Indicates whether the user requires a password to authenticate.
authentication_type :: Lens.Lens' Authentication (Prelude.Maybe AuthenticationType)
authentication_type = Lens.lens (\Authentication' {type'} -> type') (\s@Authentication' {} a -> s {type' = a} :: Authentication)

instance Prelude.FromXML Authentication where
  parseXML x =
    Authentication'
      Prelude.<$> (x Prelude..@? "PasswordCount")
      Prelude.<*> (x Prelude..@? "Type")

instance Prelude.Hashable Authentication

instance Prelude.NFData Authentication
