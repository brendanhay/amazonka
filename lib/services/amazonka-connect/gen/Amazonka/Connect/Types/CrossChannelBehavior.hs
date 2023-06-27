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
-- Module      : Amazonka.Connect.Types.CrossChannelBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.CrossChannelBehavior where

import Amazonka.Connect.Types.BehaviorType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines the cross-channel routing behavior that allows an agent working
-- on a contact in one channel to be offered a contact from a different
-- channel.
--
-- /See:/ 'newCrossChannelBehavior' smart constructor.
data CrossChannelBehavior = CrossChannelBehavior'
  { -- | Specifies the other channels that can be routed to an agent handling
    -- their current channel.
    behaviorType :: BehaviorType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CrossChannelBehavior' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'behaviorType', 'crossChannelBehavior_behaviorType' - Specifies the other channels that can be routed to an agent handling
-- their current channel.
newCrossChannelBehavior ::
  -- | 'behaviorType'
  BehaviorType ->
  CrossChannelBehavior
newCrossChannelBehavior pBehaviorType_ =
  CrossChannelBehavior'
    { behaviorType =
        pBehaviorType_
    }

-- | Specifies the other channels that can be routed to an agent handling
-- their current channel.
crossChannelBehavior_behaviorType :: Lens.Lens' CrossChannelBehavior BehaviorType
crossChannelBehavior_behaviorType = Lens.lens (\CrossChannelBehavior' {behaviorType} -> behaviorType) (\s@CrossChannelBehavior' {} a -> s {behaviorType = a} :: CrossChannelBehavior)

instance Data.FromJSON CrossChannelBehavior where
  parseJSON =
    Data.withObject
      "CrossChannelBehavior"
      ( \x ->
          CrossChannelBehavior'
            Prelude.<$> (x Data..: "BehaviorType")
      )

instance Prelude.Hashable CrossChannelBehavior where
  hashWithSalt _salt CrossChannelBehavior' {..} =
    _salt `Prelude.hashWithSalt` behaviorType

instance Prelude.NFData CrossChannelBehavior where
  rnf CrossChannelBehavior' {..} =
    Prelude.rnf behaviorType

instance Data.ToJSON CrossChannelBehavior where
  toJSON CrossChannelBehavior' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("BehaviorType" Data..= behaviorType)]
      )
