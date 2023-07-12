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
-- Module      : Amazonka.SageMaker.Types.MemberDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.MemberDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CognitoMemberDefinition
import Amazonka.SageMaker.Types.OidcMemberDefinition

-- | Defines an Amazon Cognito or your own OIDC IdP user group that is part
-- of a work team.
--
-- /See:/ 'newMemberDefinition' smart constructor.
data MemberDefinition = MemberDefinition'
  { -- | The Amazon Cognito user group that is part of the work team.
    cognitoMemberDefinition :: Prelude.Maybe CognitoMemberDefinition,
    -- | A list user groups that exist in your OIDC Identity Provider (IdP). One
    -- to ten groups can be used to create a single private work team. When you
    -- add a user group to the list of @Groups@, you can add that user group to
    -- one or more private work teams. If you add a user group to a private
    -- work team, all workers in that user group are added to the work team.
    oidcMemberDefinition :: Prelude.Maybe OidcMemberDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MemberDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cognitoMemberDefinition', 'memberDefinition_cognitoMemberDefinition' - The Amazon Cognito user group that is part of the work team.
--
-- 'oidcMemberDefinition', 'memberDefinition_oidcMemberDefinition' - A list user groups that exist in your OIDC Identity Provider (IdP). One
-- to ten groups can be used to create a single private work team. When you
-- add a user group to the list of @Groups@, you can add that user group to
-- one or more private work teams. If you add a user group to a private
-- work team, all workers in that user group are added to the work team.
newMemberDefinition ::
  MemberDefinition
newMemberDefinition =
  MemberDefinition'
    { cognitoMemberDefinition =
        Prelude.Nothing,
      oidcMemberDefinition = Prelude.Nothing
    }

-- | The Amazon Cognito user group that is part of the work team.
memberDefinition_cognitoMemberDefinition :: Lens.Lens' MemberDefinition (Prelude.Maybe CognitoMemberDefinition)
memberDefinition_cognitoMemberDefinition = Lens.lens (\MemberDefinition' {cognitoMemberDefinition} -> cognitoMemberDefinition) (\s@MemberDefinition' {} a -> s {cognitoMemberDefinition = a} :: MemberDefinition)

-- | A list user groups that exist in your OIDC Identity Provider (IdP). One
-- to ten groups can be used to create a single private work team. When you
-- add a user group to the list of @Groups@, you can add that user group to
-- one or more private work teams. If you add a user group to a private
-- work team, all workers in that user group are added to the work team.
memberDefinition_oidcMemberDefinition :: Lens.Lens' MemberDefinition (Prelude.Maybe OidcMemberDefinition)
memberDefinition_oidcMemberDefinition = Lens.lens (\MemberDefinition' {oidcMemberDefinition} -> oidcMemberDefinition) (\s@MemberDefinition' {} a -> s {oidcMemberDefinition = a} :: MemberDefinition)

instance Data.FromJSON MemberDefinition where
  parseJSON =
    Data.withObject
      "MemberDefinition"
      ( \x ->
          MemberDefinition'
            Prelude.<$> (x Data..:? "CognitoMemberDefinition")
            Prelude.<*> (x Data..:? "OidcMemberDefinition")
      )

instance Prelude.Hashable MemberDefinition where
  hashWithSalt _salt MemberDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` cognitoMemberDefinition
      `Prelude.hashWithSalt` oidcMemberDefinition

instance Prelude.NFData MemberDefinition where
  rnf MemberDefinition' {..} =
    Prelude.rnf cognitoMemberDefinition
      `Prelude.seq` Prelude.rnf oidcMemberDefinition

instance Data.ToJSON MemberDefinition where
  toJSON MemberDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CognitoMemberDefinition" Data..=)
              Prelude.<$> cognitoMemberDefinition,
            ("OidcMemberDefinition" Data..=)
              Prelude.<$> oidcMemberDefinition
          ]
      )
