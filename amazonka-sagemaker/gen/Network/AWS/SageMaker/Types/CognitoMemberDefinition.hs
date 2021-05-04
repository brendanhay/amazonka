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
-- Module      : Network.AWS.SageMaker.Types.CognitoMemberDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.CognitoMemberDefinition where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies a Amazon Cognito user group. A user group can be used in on
-- or more work teams.
--
-- /See:/ 'newCognitoMemberDefinition' smart constructor.
data CognitoMemberDefinition = CognitoMemberDefinition'
  { -- | An identifier for a user pool. The user pool must be in the same region
    -- as the service that you are calling.
    userPool :: Prelude.Text,
    -- | An identifier for a user group.
    userGroup :: Prelude.Text,
    -- | An identifier for an application client. You must create the app client
    -- ID using Amazon Cognito.
    clientId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CognitoMemberDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userPool', 'cognitoMemberDefinition_userPool' - An identifier for a user pool. The user pool must be in the same region
-- as the service that you are calling.
--
-- 'userGroup', 'cognitoMemberDefinition_userGroup' - An identifier for a user group.
--
-- 'clientId', 'cognitoMemberDefinition_clientId' - An identifier for an application client. You must create the app client
-- ID using Amazon Cognito.
newCognitoMemberDefinition ::
  -- | 'userPool'
  Prelude.Text ->
  -- | 'userGroup'
  Prelude.Text ->
  -- | 'clientId'
  Prelude.Text ->
  CognitoMemberDefinition
newCognitoMemberDefinition
  pUserPool_
  pUserGroup_
  pClientId_ =
    CognitoMemberDefinition'
      { userPool = pUserPool_,
        userGroup = pUserGroup_,
        clientId = pClientId_
      }

-- | An identifier for a user pool. The user pool must be in the same region
-- as the service that you are calling.
cognitoMemberDefinition_userPool :: Lens.Lens' CognitoMemberDefinition Prelude.Text
cognitoMemberDefinition_userPool = Lens.lens (\CognitoMemberDefinition' {userPool} -> userPool) (\s@CognitoMemberDefinition' {} a -> s {userPool = a} :: CognitoMemberDefinition)

-- | An identifier for a user group.
cognitoMemberDefinition_userGroup :: Lens.Lens' CognitoMemberDefinition Prelude.Text
cognitoMemberDefinition_userGroup = Lens.lens (\CognitoMemberDefinition' {userGroup} -> userGroup) (\s@CognitoMemberDefinition' {} a -> s {userGroup = a} :: CognitoMemberDefinition)

-- | An identifier for an application client. You must create the app client
-- ID using Amazon Cognito.
cognitoMemberDefinition_clientId :: Lens.Lens' CognitoMemberDefinition Prelude.Text
cognitoMemberDefinition_clientId = Lens.lens (\CognitoMemberDefinition' {clientId} -> clientId) (\s@CognitoMemberDefinition' {} a -> s {clientId = a} :: CognitoMemberDefinition)

instance Prelude.FromJSON CognitoMemberDefinition where
  parseJSON =
    Prelude.withObject
      "CognitoMemberDefinition"
      ( \x ->
          CognitoMemberDefinition'
            Prelude.<$> (x Prelude..: "UserPool")
            Prelude.<*> (x Prelude..: "UserGroup")
            Prelude.<*> (x Prelude..: "ClientId")
      )

instance Prelude.Hashable CognitoMemberDefinition

instance Prelude.NFData CognitoMemberDefinition

instance Prelude.ToJSON CognitoMemberDefinition where
  toJSON CognitoMemberDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserPool" Prelude..= userPool),
            Prelude.Just ("UserGroup" Prelude..= userGroup),
            Prelude.Just ("ClientId" Prelude..= clientId)
          ]
      )
