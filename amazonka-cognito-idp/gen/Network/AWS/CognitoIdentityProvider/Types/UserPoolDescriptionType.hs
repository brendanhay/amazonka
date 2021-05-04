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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolDescriptionType where

import Network.AWS.CognitoIdentityProvider.Types.LambdaConfigType
import Network.AWS.CognitoIdentityProvider.Types.StatusType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A user pool description.
--
-- /See:/ 'newUserPoolDescriptionType' smart constructor.
data UserPoolDescriptionType = UserPoolDescriptionType'
  { -- | The date the user pool description was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The user pool status in a user pool description.
    status :: Prelude.Maybe StatusType,
    -- | The ID in a user pool description.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date the user pool description was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The AWS Lambda configuration information in a user pool description.
    lambdaConfig :: Prelude.Maybe LambdaConfigType,
    -- | The name in a user pool description.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UserPoolDescriptionType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'userPoolDescriptionType_lastModifiedDate' - The date the user pool description was last modified.
--
-- 'status', 'userPoolDescriptionType_status' - The user pool status in a user pool description.
--
-- 'id', 'userPoolDescriptionType_id' - The ID in a user pool description.
--
-- 'creationDate', 'userPoolDescriptionType_creationDate' - The date the user pool description was created.
--
-- 'lambdaConfig', 'userPoolDescriptionType_lambdaConfig' - The AWS Lambda configuration information in a user pool description.
--
-- 'name', 'userPoolDescriptionType_name' - The name in a user pool description.
newUserPoolDescriptionType ::
  UserPoolDescriptionType
newUserPoolDescriptionType =
  UserPoolDescriptionType'
    { lastModifiedDate =
        Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      lambdaConfig = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The date the user pool description was last modified.
userPoolDescriptionType_lastModifiedDate :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.UTCTime)
userPoolDescriptionType_lastModifiedDate = Lens.lens (\UserPoolDescriptionType' {lastModifiedDate} -> lastModifiedDate) (\s@UserPoolDescriptionType' {} a -> s {lastModifiedDate = a} :: UserPoolDescriptionType) Prelude.. Lens.mapping Prelude._Time

-- | The user pool status in a user pool description.
userPoolDescriptionType_status :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe StatusType)
userPoolDescriptionType_status = Lens.lens (\UserPoolDescriptionType' {status} -> status) (\s@UserPoolDescriptionType' {} a -> s {status = a} :: UserPoolDescriptionType)

-- | The ID in a user pool description.
userPoolDescriptionType_id :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.Text)
userPoolDescriptionType_id = Lens.lens (\UserPoolDescriptionType' {id} -> id) (\s@UserPoolDescriptionType' {} a -> s {id = a} :: UserPoolDescriptionType)

-- | The date the user pool description was created.
userPoolDescriptionType_creationDate :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.UTCTime)
userPoolDescriptionType_creationDate = Lens.lens (\UserPoolDescriptionType' {creationDate} -> creationDate) (\s@UserPoolDescriptionType' {} a -> s {creationDate = a} :: UserPoolDescriptionType) Prelude.. Lens.mapping Prelude._Time

-- | The AWS Lambda configuration information in a user pool description.
userPoolDescriptionType_lambdaConfig :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe LambdaConfigType)
userPoolDescriptionType_lambdaConfig = Lens.lens (\UserPoolDescriptionType' {lambdaConfig} -> lambdaConfig) (\s@UserPoolDescriptionType' {} a -> s {lambdaConfig = a} :: UserPoolDescriptionType)

-- | The name in a user pool description.
userPoolDescriptionType_name :: Lens.Lens' UserPoolDescriptionType (Prelude.Maybe Prelude.Text)
userPoolDescriptionType_name = Lens.lens (\UserPoolDescriptionType' {name} -> name) (\s@UserPoolDescriptionType' {} a -> s {name = a} :: UserPoolDescriptionType)

instance Prelude.FromJSON UserPoolDescriptionType where
  parseJSON =
    Prelude.withObject
      "UserPoolDescriptionType"
      ( \x ->
          UserPoolDescriptionType'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "CreationDate")
            Prelude.<*> (x Prelude..:? "LambdaConfig")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable UserPoolDescriptionType

instance Prelude.NFData UserPoolDescriptionType
