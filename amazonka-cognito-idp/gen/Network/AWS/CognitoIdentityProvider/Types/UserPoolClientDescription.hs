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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolClientDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The description of the user pool client.
--
-- /See:/ 'newUserPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { -- | The ID of the client associated with the user pool.
    clientId :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The client name from the user pool client description.
    clientName :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool where you want to describe the user
    -- pool client.
    userPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { clientId =
        Prelude.Nothing,
      clientName = Prelude.Nothing,
      userPoolId = Prelude.Nothing
    }

-- | The ID of the client associated with the user pool.
userPoolClientDescription_clientId :: Lens.Lens' UserPoolClientDescription (Prelude.Maybe Prelude.Text)
userPoolClientDescription_clientId = Lens.lens (\UserPoolClientDescription' {clientId} -> clientId) (\s@UserPoolClientDescription' {} a -> s {clientId = a} :: UserPoolClientDescription) Prelude.. Lens.mapping Prelude._Sensitive

-- | The client name from the user pool client description.
userPoolClientDescription_clientName :: Lens.Lens' UserPoolClientDescription (Prelude.Maybe Prelude.Text)
userPoolClientDescription_clientName = Lens.lens (\UserPoolClientDescription' {clientName} -> clientName) (\s@UserPoolClientDescription' {} a -> s {clientName = a} :: UserPoolClientDescription)

-- | The user pool ID for the user pool where you want to describe the user
-- pool client.
userPoolClientDescription_userPoolId :: Lens.Lens' UserPoolClientDescription (Prelude.Maybe Prelude.Text)
userPoolClientDescription_userPoolId = Lens.lens (\UserPoolClientDescription' {userPoolId} -> userPoolId) (\s@UserPoolClientDescription' {} a -> s {userPoolId = a} :: UserPoolClientDescription)

instance Prelude.FromJSON UserPoolClientDescription where
  parseJSON =
    Prelude.withObject
      "UserPoolClientDescription"
      ( \x ->
          UserPoolClientDescription'
            Prelude.<$> (x Prelude..:? "ClientId")
            Prelude.<*> (x Prelude..:? "ClientName")
            Prelude.<*> (x Prelude..:? "UserPoolId")
      )

instance Prelude.Hashable UserPoolClientDescription

instance Prelude.NFData UserPoolClientDescription
