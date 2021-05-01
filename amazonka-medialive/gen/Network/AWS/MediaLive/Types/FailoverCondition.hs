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
-- Module      : Network.AWS.MediaLive.Types.FailoverCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverCondition where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.FailoverConditionSettings
import qualified Network.AWS.Prelude as Prelude

-- | Failover Condition settings. There can be multiple failover conditions
-- inside AutomaticInputFailoverSettings.
--
-- /See:/ 'newFailoverCondition' smart constructor.
data FailoverCondition = FailoverCondition'
  { -- | Failover condition type-specific settings.
    failoverConditionSettings :: Prelude.Maybe FailoverConditionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FailoverCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failoverConditionSettings', 'failoverCondition_failoverConditionSettings' - Failover condition type-specific settings.
newFailoverCondition ::
  FailoverCondition
newFailoverCondition =
  FailoverCondition'
    { failoverConditionSettings =
        Prelude.Nothing
    }

-- | Failover condition type-specific settings.
failoverCondition_failoverConditionSettings :: Lens.Lens' FailoverCondition (Prelude.Maybe FailoverConditionSettings)
failoverCondition_failoverConditionSettings = Lens.lens (\FailoverCondition' {failoverConditionSettings} -> failoverConditionSettings) (\s@FailoverCondition' {} a -> s {failoverConditionSettings = a} :: FailoverCondition)

instance Prelude.FromJSON FailoverCondition where
  parseJSON =
    Prelude.withObject
      "FailoverCondition"
      ( \x ->
          FailoverCondition'
            Prelude.<$> (x Prelude..:? "failoverConditionSettings")
      )

instance Prelude.Hashable FailoverCondition

instance Prelude.NFData FailoverCondition

instance Prelude.ToJSON FailoverCondition where
  toJSON FailoverCondition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("failoverConditionSettings" Prelude..=)
              Prelude.<$> failoverConditionSettings
          ]
      )
