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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The description of the user pool client.
--
-- /See:/ 'newUserPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { -- | The ID of the client associated with the user pool.
    clientId :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The client name from the user pool client description.
    clientName :: Core.Maybe Core.Text,
    -- | The user pool ID for the user pool where you want to describe the user
    -- pool client.
    userPoolId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserPoolClientDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientId', 'userPoolClientDescription_clientId' - The ID of the client associated with the user pool.
--
-- 'clientName', 'userPoolClientDescription_clientName' - The client name from the user pool client description.
--
-- 'userPoolId', 'userPoolClientDescription_userPoolId' - The user pool ID for the user pool where you want to describe the user
-- pool client.
newUserPoolClientDescription ::
  UserPoolClientDescription
newUserPoolClientDescription =
  UserPoolClientDescription'
    { clientId = Core.Nothing,
      clientName = Core.Nothing,
      userPoolId = Core.Nothing
    }

-- | The ID of the client associated with the user pool.
userPoolClientDescription_clientId :: Lens.Lens' UserPoolClientDescription (Core.Maybe Core.Text)
userPoolClientDescription_clientId = Lens.lens (\UserPoolClientDescription' {clientId} -> clientId) (\s@UserPoolClientDescription' {} a -> s {clientId = a} :: UserPoolClientDescription) Core.. Lens.mapping Core._Sensitive

-- | The client name from the user pool client description.
userPoolClientDescription_clientName :: Lens.Lens' UserPoolClientDescription (Core.Maybe Core.Text)
userPoolClientDescription_clientName = Lens.lens (\UserPoolClientDescription' {clientName} -> clientName) (\s@UserPoolClientDescription' {} a -> s {clientName = a} :: UserPoolClientDescription)

-- | The user pool ID for the user pool where you want to describe the user
-- pool client.
userPoolClientDescription_userPoolId :: Lens.Lens' UserPoolClientDescription (Core.Maybe Core.Text)
userPoolClientDescription_userPoolId = Lens.lens (\UserPoolClientDescription' {userPoolId} -> userPoolId) (\s@UserPoolClientDescription' {} a -> s {userPoolId = a} :: UserPoolClientDescription)

instance Core.FromJSON UserPoolClientDescription where
  parseJSON =
    Core.withObject
      "UserPoolClientDescription"
      ( \x ->
          UserPoolClientDescription'
            Core.<$> (x Core..:? "ClientId")
            Core.<*> (x Core..:? "ClientName")
            Core.<*> (x Core..:? "UserPoolId")
      )

instance Core.Hashable UserPoolClientDescription

instance Core.NFData UserPoolClientDescription
