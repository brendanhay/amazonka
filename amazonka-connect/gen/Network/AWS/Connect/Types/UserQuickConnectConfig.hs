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
-- Module      : Network.AWS.Connect.Types.UserQuickConnectConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.UserQuickConnectConfig where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the quick connect configuration settings for
-- a user. The contact flow must be of type Transfer to Agent.
--
-- /See:/ 'newUserQuickConnectConfig' smart constructor.
data UserQuickConnectConfig = UserQuickConnectConfig'
  { -- | The identifier of the user.
    userId :: Core.Text,
    -- | The identifier of the contact flow.
    contactFlowId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UserQuickConnectConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userId', 'userQuickConnectConfig_userId' - The identifier of the user.
--
-- 'contactFlowId', 'userQuickConnectConfig_contactFlowId' - The identifier of the contact flow.
newUserQuickConnectConfig ::
  -- | 'userId'
  Core.Text ->
  -- | 'contactFlowId'
  Core.Text ->
  UserQuickConnectConfig
newUserQuickConnectConfig pUserId_ pContactFlowId_ =
  UserQuickConnectConfig'
    { userId = pUserId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier of the user.
userQuickConnectConfig_userId :: Lens.Lens' UserQuickConnectConfig Core.Text
userQuickConnectConfig_userId = Lens.lens (\UserQuickConnectConfig' {userId} -> userId) (\s@UserQuickConnectConfig' {} a -> s {userId = a} :: UserQuickConnectConfig)

-- | The identifier of the contact flow.
userQuickConnectConfig_contactFlowId :: Lens.Lens' UserQuickConnectConfig Core.Text
userQuickConnectConfig_contactFlowId = Lens.lens (\UserQuickConnectConfig' {contactFlowId} -> contactFlowId) (\s@UserQuickConnectConfig' {} a -> s {contactFlowId = a} :: UserQuickConnectConfig)

instance Core.FromJSON UserQuickConnectConfig where
  parseJSON =
    Core.withObject
      "UserQuickConnectConfig"
      ( \x ->
          UserQuickConnectConfig'
            Core.<$> (x Core..: "UserId")
            Core.<*> (x Core..: "ContactFlowId")
      )

instance Core.Hashable UserQuickConnectConfig

instance Core.NFData UserQuickConnectConfig

instance Core.ToJSON UserQuickConnectConfig where
  toJSON UserQuickConnectConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("UserId" Core..= userId),
            Core.Just ("ContactFlowId" Core..= contactFlowId)
          ]
      )
