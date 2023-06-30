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
-- Module      : Amazonka.CognitoIdentityProvider.Types.UserPoolDescriptionType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.UserPoolDescriptionType where

import Amazonka.CognitoIdentityProvider.Types.LambdaConfigType
import Amazonka.CognitoIdentityProvider.Types.StatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A user pool description.
--
-- /See:/ 'newUserPoolDescriptionType' smart constructor.
data UserPoolDescriptionType = UserPoolDescriptionType'
  { -- | The date the user pool description was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The ID in a user pool description.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Lambda configuration information in a user pool description.
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | The date the user pool description was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The name in a user pool description.
    name :: Prelude.Maybe Prelude.Text,
    -- | The user pool status in a user pool description.
    status :: Prelude.Maybe StatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPoolDescriptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'userPoolDescriptionType_creationDate' - The date the user pool description was created.
--
-- 'id', 'userPoolDescriptionType_id' - The ID in a user pool description.
--
-- 'lambdaConfig', 'userPoolDescriptionType_lambdaConfig' - The Lambda configuration information in a user pool description.
--
-- 'lastModifiedDate', 'userPoolDescriptionType_lastModifiedDate' - The date the user pool description was last modified.
--
-- 'name', 'userPoolDescriptionType_name' - The name in a user pool description.
--
-- 'status', 'userPoolDescriptionType_status' - The user pool status in a user pool description.
newUserPoolDescriptionType ::
  UserPoolDescriptionType
newUserPoolDescriptionType =
  UserPoolDescriptionType'
    { creationDate =
        Prelude.Nothing,
      id = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The date the user pool description was created.
userPoolDescriptionType_creationDate :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.UTCTime)
userPoolDescriptionType_creationDate = Lens.lens (\UserPoolDescriptionType' {creationDate} -> creationDate) (\s@UserPoolDescriptionType' {} a -> s {creationDate = a} :: UserPoolDescriptionType) Prelude.. Lens.mapping Data._Time

-- | The ID in a user pool description.
userPoolDescriptionType_id :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.Text)
userPoolDescriptionType_id = Lens.lens (\UserPoolDescriptionType' {id} -> id) (\s@UserPoolDescriptionType' {} a -> s {id = a} :: UserPoolDescriptionType)

-- | The Lambda configuration information in a user pool description.
userPoolDescriptionType_lambdaConfig :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe LambdaConfigType)
userPoolDescriptionType_lambdaConfig = Lens.lens (\UserPoolDescriptionType' {lambdaConfig} -> lambdaConfig) (\s@UserPoolDescriptionType' {} a -> s {lambdaConfig = a} :: UserPoolDescriptionType)

-- | The date the user pool description was last modified.
userPoolDescriptionType_lastModifiedDate :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.UTCTime)
userPoolDescriptionType_lastModifiedDate = Lens.lens (\UserPoolDescriptionType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolDescriptionType' {} a -> s {lastModifiedDate = a} :: UserPoolDescriptionType) Prelude.. Lens.mapping Data._Time

-- | The name in a user pool description.
userPoolDescriptionType_name :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.Text)
userPoolDescriptionType_name = Lens.lens (\UserPoolDescriptionType' {name} -> name) (\s@UserPoolDescriptionType' {} a -> s {name = a} :: UserPoolDescriptionType)

-- | The user pool status in a user pool description.
userPoolDescriptionType_status :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe StatusType)
userPoolDescriptionType_status = Lens.lens (\UserPoolDescriptionType' {status} -> status) (\s@UserPoolDescriptionType' {} a -> s {status = a} :: UserPoolDescriptionType)

instance Data.FromJSON UserPoolDescriptionType where
  parseJSON =
    Data.withObject
      "UserPoolDescriptionType"
      ( \x ->
          UserPoolDescriptionType'
            Prelude.<$> (x Data..:? "CreationDate")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "LambdaConfig")
            Prelude.<*> (x Data..:? "LastModifiedDate")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable UserPoolDescriptionType where
  hashWithSalt _salt UserPoolDescriptionType' {..} =
    _salt
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lambdaConfig
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData UserPoolDescriptionType where
  rnf UserPoolDescriptionType' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lambdaConfig
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
