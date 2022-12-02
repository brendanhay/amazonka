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
-- Module      : Amazonka.Connect.Types.UserQuickConnectConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserQuickConnectConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the quick connect configuration settings for
-- a user. The contact flow must be of type Transfer to Agent.
--
-- /See:/ 'newUserQuickConnectConfig' smart constructor.
data UserQuickConnectConfig = UserQuickConnectConfig'
  { -- | The identifier of the user.
    userId :: Prelude.Text,
    -- | The identifier of the flow.
    contactFlowId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'contactFlowId', 'userQuickConnectConfig_contactFlowId' - The identifier of the flow.
newUserQuickConnectConfig ::
  -- | 'userId'
  Prelude.Text ->
  -- | 'contactFlowId'
  Prelude.Text ->
  UserQuickConnectConfig
newUserQuickConnectConfig pUserId_ pContactFlowId_ =
  UserQuickConnectConfig'
    { userId = pUserId_,
      contactFlowId = pContactFlowId_
    }

-- | The identifier of the user.
userQuickConnectConfig_userId :: Lens.Lens' UserQuickConnectConfig Prelude.Text
userQuickConnectConfig_userId = Lens.lens (\UserQuickConnectConfig' {userId} -> userId) (\s@UserQuickConnectConfig' {} a -> s {userId = a} :: UserQuickConnectConfig)

-- | The identifier of the flow.
userQuickConnectConfig_contactFlowId :: Lens.Lens' UserQuickConnectConfig Prelude.Text
userQuickConnectConfig_contactFlowId = Lens.lens (\UserQuickConnectConfig' {contactFlowId} -> contactFlowId) (\s@UserQuickConnectConfig' {} a -> s {contactFlowId = a} :: UserQuickConnectConfig)

instance Data.FromJSON UserQuickConnectConfig where
  parseJSON =
    Data.withObject
      "UserQuickConnectConfig"
      ( \x ->
          UserQuickConnectConfig'
            Prelude.<$> (x Data..: "UserId")
            Prelude.<*> (x Data..: "ContactFlowId")
      )

instance Prelude.Hashable UserQuickConnectConfig where
  hashWithSalt _salt UserQuickConnectConfig' {..} =
    _salt `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` contactFlowId

instance Prelude.NFData UserQuickConnectConfig where
  rnf UserQuickConnectConfig' {..} =
    Prelude.rnf userId
      `Prelude.seq` Prelude.rnf contactFlowId

instance Data.ToJSON UserQuickConnectConfig where
  toJSON UserQuickConnectConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("UserId" Data..= userId),
            Prelude.Just
              ("ContactFlowId" Data..= contactFlowId)
          ]
      )
