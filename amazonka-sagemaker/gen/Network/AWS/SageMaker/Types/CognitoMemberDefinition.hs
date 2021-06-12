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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifies a Amazon Cognito user group. A user group can be used in on
-- or more work teams.
--
-- /See:/ 'newCognitoMemberDefinition' smart constructor.
data CognitoMemberDefinition = CognitoMemberDefinition'
  { -- | An identifier for a user pool. The user pool must be in the same region
    -- as the service that you are calling.
    userPool :: Core.Text,
    -- | An identifier for a user group.
    userGroup :: Core.Text,
    -- | An identifier for an application client. You must create the app client
    -- ID using Amazon Cognito.
    clientId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 'userGroup'
  Core.Text ->
  -- | 'clientId'
  Core.Text ->
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
cognitoMemberDefinition_userPool :: Lens.Lens' CognitoMemberDefinition Core.Text
cognitoMemberDefinition_userPool = Lens.lens (\CognitoMemberDefinition' {userPool} -> userPool) (\s@CognitoMemberDefinition' {} a -> s {userPool = a} :: CognitoMemberDefinition)

-- | An identifier for a user group.
cognitoMemberDefinition_userGroup :: Lens.Lens' CognitoMemberDefinition Core.Text
cognitoMemberDefinition_userGroup = Lens.lens (\CognitoMemberDefinition' {userGroup} -> userGroup) (\s@CognitoMemberDefinition' {} a -> s {userGroup = a} :: CognitoMemberDefinition)

-- | An identifier for an application client. You must create the app client
-- ID using Amazon Cognito.
cognitoMemberDefinition_clientId :: Lens.Lens' CognitoMemberDefinition Core.Text
cognitoMemberDefinition_clientId = Lens.lens (\CognitoMemberDefinition' {clientId} -> clientId) (\s@CognitoMemberDefinition' {} a -> s {clientId = a} :: CognitoMemberDefinition)

instance Core.FromJSON CognitoMemberDefinition where
  parseJSON =
    Core.withObject
      "CognitoMemberDefinition"
      ( \x ->
          CognitoMemberDefinition'
            Core.<$> (x Core..: "UserPool")
            Core.<*> (x Core..: "UserGroup")
            Core.<*> (x Core..: "ClientId")
      )

instance Core.Hashable CognitoMemberDefinition

instance Core.NFData CognitoMemberDefinition

instance Core.ToJSON CognitoMemberDefinition where
  toJSON CognitoMemberDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserPool" Core..= userPool),
            Core.Just ("UserGroup" Core..= userGroup),
            Core.Just ("ClientId" Core..= clientId)
          ]
      )
