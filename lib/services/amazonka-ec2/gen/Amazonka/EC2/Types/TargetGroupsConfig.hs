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
-- Module      : Amazonka.EC2.Types.TargetGroupsConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetGroupsConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.TargetGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes the target groups to attach to a Spot Fleet. Spot Fleet
-- registers the running Spot Instances with these target groups.
--
-- /See:/ 'newTargetGroupsConfig' smart constructor.
data TargetGroupsConfig = TargetGroupsConfig'
  { -- | One or more target groups.
    targetGroups :: Prelude.Maybe (Prelude.NonEmpty TargetGroup)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupsConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'targetGroupsConfig_targetGroups' - One or more target groups.
newTargetGroupsConfig ::
  TargetGroupsConfig
newTargetGroupsConfig =
  TargetGroupsConfig' {targetGroups = Prelude.Nothing}

-- | One or more target groups.
targetGroupsConfig_targetGroups :: Lens.Lens' TargetGroupsConfig (Prelude.Maybe (Prelude.NonEmpty TargetGroup))
targetGroupsConfig_targetGroups = Lens.lens (\TargetGroupsConfig' {targetGroups} -> targetGroups) (\s@TargetGroupsConfig' {} a -> s {targetGroups = a} :: TargetGroupsConfig) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML TargetGroupsConfig where
  parseXML x =
    TargetGroupsConfig'
      Prelude.<$> ( x Data..@? "targetGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList1 "item")
                  )

instance Prelude.Hashable TargetGroupsConfig where
  hashWithSalt _salt TargetGroupsConfig' {..} =
    _salt `Prelude.hashWithSalt` targetGroups

instance Prelude.NFData TargetGroupsConfig where
  rnf TargetGroupsConfig' {..} =
    Prelude.rnf targetGroups

instance Data.ToQuery TargetGroupsConfig where
  toQuery TargetGroupsConfig' {..} =
    Prelude.mconcat
      [ Data.toQuery
          ( Data.toQueryList "TargetGroups"
              Prelude.<$> targetGroups
          )
      ]
