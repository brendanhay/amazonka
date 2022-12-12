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
-- Module      : Amazonka.ElasticSearch.Types.AutoTune
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.AutoTune where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.AutoTuneDetails
import Amazonka.ElasticSearch.Types.AutoTuneType
import qualified Amazonka.Prelude as Prelude

-- | Specifies Auto-Tune type and Auto-Tune action details.
--
-- /See:/ 'newAutoTune' smart constructor.
data AutoTune = AutoTune'
  { -- | Specifies details of the Auto-Tune action. See the
    -- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
    -- for more information.
    autoTuneDetails :: Prelude.Maybe AutoTuneDetails,
    -- | Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
    autoTuneType :: Prelude.Maybe AutoTuneType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTune' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoTuneDetails', 'autoTune_autoTuneDetails' - Specifies details of the Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- 'autoTuneType', 'autoTune_autoTuneType' - Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
newAutoTune ::
  AutoTune
newAutoTune =
  AutoTune'
    { autoTuneDetails = Prelude.Nothing,
      autoTuneType = Prelude.Nothing
    }

-- | Specifies details of the Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
autoTune_autoTuneDetails :: Lens.Lens' AutoTune (Prelude.Maybe AutoTuneDetails)
autoTune_autoTuneDetails = Lens.lens (\AutoTune' {autoTuneDetails} -> autoTuneDetails) (\s@AutoTune' {} a -> s {autoTuneDetails = a} :: AutoTune)

-- | Specifies Auto-Tune type. Valid value is SCHEDULED_ACTION.
autoTune_autoTuneType :: Lens.Lens' AutoTune (Prelude.Maybe AutoTuneType)
autoTune_autoTuneType = Lens.lens (\AutoTune' {autoTuneType} -> autoTuneType) (\s@AutoTune' {} a -> s {autoTuneType = a} :: AutoTune)

instance Data.FromJSON AutoTune where
  parseJSON =
    Data.withObject
      "AutoTune"
      ( \x ->
          AutoTune'
            Prelude.<$> (x Data..:? "AutoTuneDetails")
            Prelude.<*> (x Data..:? "AutoTuneType")
      )

instance Prelude.Hashable AutoTune where
  hashWithSalt _salt AutoTune' {..} =
    _salt `Prelude.hashWithSalt` autoTuneDetails
      `Prelude.hashWithSalt` autoTuneType

instance Prelude.NFData AutoTune where
  rnf AutoTune' {..} =
    Prelude.rnf autoTuneDetails
      `Prelude.seq` Prelude.rnf autoTuneType
