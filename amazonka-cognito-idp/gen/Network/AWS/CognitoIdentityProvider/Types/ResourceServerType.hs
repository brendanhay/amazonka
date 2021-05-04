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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.ResourceServerType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.ResourceServerType where

import Network.AWS.CognitoIdentityProvider.Types.ResourceServerScopeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A container for information about a resource server for a user pool.
--
-- /See:/ 'newResourceServerType' smart constructor.
data ResourceServerType = ResourceServerType'
  { -- | A list of scopes that are defined for the resource server.
    scopes :: Prelude.Maybe [ResourceServerScopeType],
    -- | The identifier for the resource server.
    identifier :: Prelude.Maybe Prelude.Text,
    -- | The user pool ID for the user pool that hosts the resource server.
    userPoolId :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource server.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ResourceServerType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scopes', 'resourceServerType_scopes' - A list of scopes that are defined for the resource server.
--
-- 'identifier', 'resourceServerType_identifier' - The identifier for the resource server.
--
-- 'userPoolId', 'resourceServerType_userPoolId' - The user pool ID for the user pool that hosts the resource server.
--
-- 'name', 'resourceServerType_name' - The name of the resource server.
newResourceServerType ::
  ResourceServerType
newResourceServerType =
  ResourceServerType'
    { scopes = Prelude.Nothing,
      identifier = Prelude.Nothing,
      userPoolId = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | A list of scopes that are defined for the resource server.
resourceServerType_scopes :: Lens.Lens' ResourceServerType (Prelude.Maybe [ResourceServerScopeType])
resourceServerType_scopes = Lens.lens (\ResourceServerType' {scopes} -> scopes) (\s@ResourceServerType' {} a -> s {scopes = a} :: ResourceServerType) Prelude.. Lens.mapping Prelude._Coerce

-- | The identifier for the resource server.
resourceServerType_identifier :: Lens.Lens' ResourceServerType (Prelude.Maybe Prelude.Text)
resourceServerType_identifier = Lens.lens (\ResourceServerType' {identifier} -> identifier) (\s@ResourceServerType' {} a -> s {identifier = a} :: ResourceServerType)

-- | The user pool ID for the user pool that hosts the resource server.
resourceServerType_userPoolId :: Lens.Lens' ResourceServerType (Prelude.Maybe Prelude.Text)
resourceServerType_userPoolId = Lens.lens (\ResourceServerType' {userPoolId} -> userPoolId) (\s@ResourceServerType' {} a -> s {userPoolId = a} :: ResourceServerType)

-- | The name of the resource server.
resourceServerType_name :: Lens.Lens' ResourceServerType (Prelude.Maybe Prelude.Text)
resourceServerType_name = Lens.lens (\ResourceServerType' {name} -> name) (\s@ResourceServerType' {} a -> s {name = a} :: ResourceServerType)

instance Prelude.FromJSON ResourceServerType where
  parseJSON =
    Prelude.withObject
      "ResourceServerType"
      ( \x ->
          ResourceServerType'
            Prelude.<$> (x Prelude..:? "Scopes" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Identifier")
            Prelude.<*> (x Prelude..:? "UserPoolId")
            Prelude.<*> (x Prelude..:? "Name")
      )

instance Prelude.Hashable ResourceServerType

instance Prelude.NFData ResourceServerType
