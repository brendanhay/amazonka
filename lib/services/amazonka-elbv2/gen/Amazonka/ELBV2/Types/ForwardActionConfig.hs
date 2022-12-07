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
-- Module      : Amazonka.ELBV2.Types.ForwardActionConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.ForwardActionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types.TargetGroupStickinessConfig
import Amazonka.ELBV2.Types.TargetGroupTuple
import qualified Amazonka.Prelude as Prelude

-- | Information about a forward action.
--
-- /See:/ 'newForwardActionConfig' smart constructor.
data ForwardActionConfig = ForwardActionConfig'
  { -- | The target group stickiness for the rule.
    targetGroupStickinessConfig :: Prelude.Maybe TargetGroupStickinessConfig,
    -- | The target groups. For Network Load Balancers, you can specify a single
    -- target group.
    targetGroups :: Prelude.Maybe [TargetGroupTuple]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForwardActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroupStickinessConfig', 'forwardActionConfig_targetGroupStickinessConfig' - The target group stickiness for the rule.
--
-- 'targetGroups', 'forwardActionConfig_targetGroups' - The target groups. For Network Load Balancers, you can specify a single
-- target group.
newForwardActionConfig ::
  ForwardActionConfig
newForwardActionConfig =
  ForwardActionConfig'
    { targetGroupStickinessConfig =
        Prelude.Nothing,
      targetGroups = Prelude.Nothing
    }

-- | The target group stickiness for the rule.
forwardActionConfig_targetGroupStickinessConfig :: Lens.Lens' ForwardActionConfig (Prelude.Maybe TargetGroupStickinessConfig)
forwardActionConfig_targetGroupStickinessConfig = Lens.lens (\ForwardActionConfig' {targetGroupStickinessConfig} -> targetGroupStickinessConfig) (\s@ForwardActionConfig' {} a -> s {targetGroupStickinessConfig = a} :: ForwardActionConfig)

-- | The target groups. For Network Load Balancers, you can specify a single
-- target group.
forwardActionConfig_targetGroups :: Lens.Lens' ForwardActionConfig (Prelude.Maybe [TargetGroupTuple])
forwardActionConfig_targetGroups = Lens.lens (\ForwardActionConfig' {targetGroups} -> targetGroups) (\s@ForwardActionConfig' {} a -> s {targetGroups = a} :: ForwardActionConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ForwardActionConfig where
  parseXML x =
    ForwardActionConfig'
      Prelude.<$> (x Data..@? "TargetGroupStickinessConfig")
      Prelude.<*> ( x Data..@? "TargetGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "member")
                  )

instance Prelude.Hashable ForwardActionConfig where
  hashWithSalt _salt ForwardActionConfig' {..} =
    _salt
      `Prelude.hashWithSalt` targetGroupStickinessConfig
      `Prelude.hashWithSalt` targetGroups

instance Prelude.NFData ForwardActionConfig where
  rnf ForwardActionConfig' {..} =
    Prelude.rnf targetGroupStickinessConfig
      `Prelude.seq` Prelude.rnf targetGroups

instance Data.ToQuery ForwardActionConfig where
  toQuery ForwardActionConfig' {..} =
    Prelude.mconcat
      [ "TargetGroupStickinessConfig"
          Data.=: targetGroupStickinessConfig,
        "TargetGroups"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> targetGroups)
      ]
