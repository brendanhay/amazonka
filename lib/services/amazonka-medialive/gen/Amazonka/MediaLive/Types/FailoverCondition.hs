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
-- Module      : Amazonka.MediaLive.Types.FailoverCondition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.FailoverCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaLive.Types.FailoverConditionSettings
import qualified Amazonka.Prelude as Prelude

-- | Failover Condition settings. There can be multiple failover conditions
-- inside AutomaticInputFailoverSettings.
--
-- /See:/ 'newFailoverCondition' smart constructor.
data FailoverCondition = FailoverCondition'
  { -- | Failover condition type-specific settings.
    failoverConditionSettings :: Prelude.Maybe FailoverConditionSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON FailoverCondition where
  parseJSON =
    Data.withObject
      "FailoverCondition"
      ( \x ->
          FailoverCondition'
            Prelude.<$> (x Data..:? "failoverConditionSettings")
      )

instance Prelude.Hashable FailoverCondition where
  hashWithSalt _salt FailoverCondition' {..} =
    _salt
      `Prelude.hashWithSalt` failoverConditionSettings

instance Prelude.NFData FailoverCondition where
  rnf FailoverCondition' {..} =
    Prelude.rnf failoverConditionSettings

instance Data.ToJSON FailoverCondition where
  toJSON FailoverCondition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("failoverConditionSettings" Data..=)
              Prelude.<$> failoverConditionSettings
          ]
      )
