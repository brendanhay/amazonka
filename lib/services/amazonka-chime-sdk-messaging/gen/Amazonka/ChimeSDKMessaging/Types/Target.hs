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
-- Module      : Amazonka.ChimeSDKMessaging.Types.Target
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ChimeSDKMessaging.Types.Target where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The target of a message, a sender, a user, or a bot. Only the target and
-- the sender can view targeted messages. Only users who can see targeted
-- messages can take actions on them. However, administrators can delete
-- targeted messages that they can’t see.
--
-- /See:/ 'newTarget' smart constructor.
data Target = Target'
  { -- | The ARN of the target channel member.
    memberArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Target' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memberArn', 'target_memberArn' - The ARN of the target channel member.
newTarget ::
  Target
newTarget = Target' {memberArn = Prelude.Nothing}

-- | The ARN of the target channel member.
target_memberArn :: Lens.Lens' Target (Prelude.Maybe Prelude.Text)
target_memberArn = Lens.lens (\Target' {memberArn} -> memberArn) (\s@Target' {} a -> s {memberArn = a} :: Target)

instance Data.FromJSON Target where
  parseJSON =
    Data.withObject
      "Target"
      (\x -> Target' Prelude.<$> (x Data..:? "MemberArn"))

instance Prelude.Hashable Target where
  hashWithSalt _salt Target' {..} =
    _salt `Prelude.hashWithSalt` memberArn

instance Prelude.NFData Target where
  rnf Target' {..} = Prelude.rnf memberArn

instance Data.ToJSON Target where
  toJSON Target' {..} =
    Data.object
      ( Prelude.catMaybes
          [("MemberArn" Data..=) Prelude.<$> memberArn]
      )
