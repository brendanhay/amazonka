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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTune
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTune where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types.AutoTuneDetails
import Network.AWS.ElasticSearch.Types.AutoTuneType
import qualified Network.AWS.Lens as Lens

-- | Specifies Auto-Tune type and Auto-Tune action details.
--
-- /See:/ 'newAutoTune' smart constructor.
data AutoTune = AutoTune'
  { -- | Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
    autoTuneType :: Core.Maybe AutoTuneType,
    -- | Specifies details of the Auto-Tune action. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    autoTuneDetails :: Core.Maybe AutoTuneDetails
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutoTune' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoTuneType', 'autoTune_autoTuneType' - Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
--
-- 'autoTuneDetails', 'autoTune_autoTuneDetails' - Specifies details of the Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
newAutoTune ::
  AutoTune
newAutoTune =
  AutoTune'
    { autoTuneType = Core.Nothing,
      autoTuneDetails = Core.Nothing
    }

-- | Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
autoTune_autoTuneType :: Lens.Lens' AutoTune (Core.Maybe AutoTuneType)
autoTune_autoTuneType = Lens.lens (\AutoTune' {autoTuneType} -> autoTuneType) (\s@AutoTune' {} a -> s {autoTuneType = a} :: AutoTune)

-- | Specifies details of the Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTune_autoTuneDetails :: Lens.Lens' AutoTune (Core.Maybe AutoTuneDetails)
autoTune_autoTuneDetails = Lens.lens (\AutoTune' {autoTuneDetails} -> autoTuneDetails) (\s@AutoTune' {} a -> s {autoTuneDetails = a} :: AutoTune)

instance Core.FromJSON AutoTune where
  parseJSON =
    Core.withObject
      "AutoTune"
      ( \x ->
          AutoTune'
            Core.<$> (x Core..:? "AutoTuneType")
            Core.<*> (x Core..:? "AutoTuneDetails")
      )

instance Core.Hashable AutoTune

instance Core.NFData AutoTune
