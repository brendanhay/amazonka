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
-- Module      : Network.AWS.SageMaker.Types.MemberDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MemberDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
import Network.AWS.SageMaker.Types.OidcMemberDefinition

-- | Defines an Amazon Cognito or your own OIDC IdP user group that is part
-- of a work team.
--
-- /See:/ 'newMemberDefinition' smart constructor.
data MemberDefinition = MemberDefinition'
  { -- | A list user groups that exist in your OIDC Identity Provider (IdP). One
    -- to ten groups can be used to create a single private work team. When you
    -- add a user group to the list of @Groups@, you can add that user group to
    -- one or more private work teams. If you add a user group to a private
    -- work team, all workers in that user group are added to the work team.
    oidcMemberDefinition :: Prelude.Maybe OidcMemberDefinition,
    -- | The Amazon Cognito user group that is part of the work team.
    cognitoMemberDefinition :: Prelude.Maybe CognitoMemberDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MemberDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oidcMemberDefinition', 'memberDefinition_oidcMemberDefinition' - A list user groups that exist in your OIDC Identity Provider (IdP). One
-- to ten groups can be used to create a single private work team. When you
-- add a user group to the list of @Groups@, you can add that user group to
-- one or more private work teams. If you add a user group to a private
-- work team, all workers in that user group are added to the work team.
--
-- 'cognitoMemberDefinition', 'memberDefinition_cognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
newMemberDefinition ::
  MemberDefinition
newMemberDefinition =
  MemberDefinition'
    { oidcMemberDefinition =
        Prelude.Nothing,
      cognitoMemberDefinition = Prelude.Nothing
    }

-- | A list user groups that exist in your OIDC Identity Provider (IdP). One
-- to ten groups can be used to create a single private work team. When you
-- add a user group to the list of @Groups@, you can add that user group to
-- one or more private work teams. If you add a user group to a private
-- work team, all workers in that user group are added to the work team.
memberDefinition_oidcMemberDefinition :: Lens.Lens' MemberDefinition (Prelude.Maybe OidcMemberDefinition)
memberDefinition_oidcMemberDefinition = Lens.lens (\MemberDefinition' {oidcMemberDefinition} -> oidcMemberDefinition) (\s@MemberDefinition' {} a -> s {oidcMemberDefinition = a} :: MemberDefinition)

-- | The Amazon Cognito user group that is part of the work team.
memberDefinition_cognitoMemberDefinition :: Lens.Lens' MemberDefinition (Prelude.Maybe CognitoMemberDefinition)
memberDefinition_cognitoMemberDefinition = Lens.lens (\MemberDefinition' {cognitoMemberDefinition} -> cognitoMemberDefinition) (\s@MemberDefinition' {} a -> s {cognitoMemberDefinition = a} :: MemberDefinition)

instance Prelude.FromJSON MemberDefinition where
  parseJSON =
    Prelude.withObject
      "MemberDefinition"
      ( \x ->
          MemberDefinition'
            Prelude.<$> (x Prelude..:? "OidcMemberDefinition")
            Prelude.<*> (x Prelude..:? "CognitoMemberDefinition")
      )

instance Prelude.Hashable MemberDefinition

instance Prelude.NFData MemberDefinition

instance Prelude.ToJSON MemberDefinition where
  toJSON MemberDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("OidcMemberDefinition" Prelude..=)
              Prelude.<$> oidcMemberDefinition,
            ("CognitoMemberDefinition" Prelude..=)
              Prelude.<$> cognitoMemberDefinition
          ]
      )
