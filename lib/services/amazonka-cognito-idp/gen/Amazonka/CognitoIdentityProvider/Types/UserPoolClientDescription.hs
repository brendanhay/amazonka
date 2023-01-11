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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolClientDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolClientDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The description of the user pool client.
--
-- /See:/ 'newUserPoolClientDescription' smart constructor.
data UserPoolClientDescription = UserPoolClientDescription'
  { -- | The ID of the client associated with the user pool.
    clientId :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The client name from the user pool client description.
    clientName :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool where you want to describe the user
    -- pool client.
    userPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
userPoolClientDescription_clientId = Lens.lens (\UserPoolClientDescription' {clientId} -> clientId) (\s@UserPoolClientDescription' {} a -> s {clientId = a} :: UserPoolClientDescription) Prelude.. Lens.mapping Data._Sensitive

-- | The client name from the user pool client description.
userPoolClientDescription_clientName :: Lens.Lens' UserPoolClientDescription (Prelude.Maybe Prelude.Text)
userPoolClientDescription_clientName = Lens.lens (\UserPoolClientDescription' {clientName} -> clientName) (\s@UserPoolClientDescription' {} a -> s {clientName = a} :: UserPoolClientDescription)

-- | The user pool ID for the user pool where you want to describe the user
-- pool client.
userPoolClientDescription_userPoolId :: Lens.Lens' UserPoolClientDescription (Prelude.Maybe Prelude.Text)
userPoolClientDescription_userPoolId = Lens.lens (\UserPoolClientDescription' {userPoolId} -> userPoolId) (\s@UserPoolClientDescription' {} a -> s {userPoolId = a} :: UserPoolClientDescription)

instance Data.FromJSON UserPoolClientDescription where
  parseJSON =
    Data.withObject
      "UserPoolClientDescription"
      ( \x ->
          UserPoolClientDescription'
            Prelude.<$> (x Data..:? "ClientId")
            Prelude.<*> (x Data..:? "ClientName")
            Prelude.<*> (x Data..:? "UserPoolId")
      )

instance Prelude.Hashable UserPoolClientDescription where
  hashWithSalt _salt UserPoolClientDescription' {..} =
    _salt `Prelude.hashWithSalt` clientId
      `Prelude.hashWithSalt` clientName
      `Prelude.hashWithSalt` userPoolId

instance Prelude.NFData UserPoolClientDescription where
  rnf UserPoolClientDescription' {..} =
    Prelude.rnf clientId
      `Prelude.seq` Prelude.rnf clientName
      `Prelude.seq` Prelude.rnf userPoolId
