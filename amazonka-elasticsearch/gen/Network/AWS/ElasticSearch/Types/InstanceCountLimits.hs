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
-- Module      : Network.AWS.ElasticSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.InstanceCountLimits where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | InstanceCountLimits represents the limits on number of instances that be
-- created in Amazon Elasticsearch for given InstanceType.
--
-- /See:/ 'newInstanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { maximumInstanceCount :: Core.Maybe Core.Int,
    minimumInstanceCount :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceCountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumInstanceCount', 'instanceCountLimits_maximumInstanceCount' - Undocumented member.
--
-- 'minimumInstanceCount', 'instanceCountLimits_minimumInstanceCount' - Undocumented member.
newInstanceCountLimits ::
  InstanceCountLimits
newInstanceCountLimits =
  InstanceCountLimits'
    { maximumInstanceCount =
        Core.Nothing,
      minimumInstanceCount = Core.Nothing
    }

-- | Undocumented member.
instanceCountLimits_maximumInstanceCount :: Lens.Lens' InstanceCountLimits (Core.Maybe Core.Int)
instanceCountLimits_maximumInstanceCount = Lens.lens (\InstanceCountLimits' {maximumInstanceCount} -> maximumInstanceCount) (\s@InstanceCountLimits' {} a -> s {maximumInstanceCount = a} :: InstanceCountLimits)

-- | Undocumented member.
instanceCountLimits_minimumInstanceCount :: Lens.Lens' InstanceCountLimits (Core.Maybe Core.Int)
instanceCountLimits_minimumInstanceCount = Lens.lens (\InstanceCountLimits' {minimumInstanceCount} -> minimumInstanceCount) (\s@InstanceCountLimits' {} a -> s {minimumInstanceCount = a} :: InstanceCountLimits)

instance Core.FromJSON InstanceCountLimits where
  parseJSON =
    Core.withObject
      "InstanceCountLimits"
      ( \x ->
          InstanceCountLimits'
            Core.<$> (x Core..:? "MaximumInstanceCount")
            Core.<*> (x Core..:? "MinimumInstanceCount")
      )

instance Core.Hashable InstanceCountLimits

instance Core.NFData InstanceCountLimits
