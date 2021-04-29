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
-- Module      : Network.AWS.ElasticSearch.Types.AutoTuneDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.AutoTuneDetails where

import Network.AWS.ElasticSearch.Types.ScheduledAutoTuneDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies details of the Auto-Tune action. See the
-- <https://docs.aws.amazon.com/elasticsearch-service/latest/developerguide/auto-tune.html Developer Guide>
-- for more information.
--
-- /See:/ 'newAutoTuneDetails' smart constructor.
data AutoTuneDetails = AutoTuneDetails'
  { scheduledAutoTuneDetails :: Prelude.Maybe ScheduledAutoTuneDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAutoTuneDetails', 'autoTuneDetails_scheduledAutoTuneDetails' - Undocumented member.
newAutoTuneDetails ::
  AutoTuneDetails
newAutoTuneDetails =
  AutoTuneDetails'
    { scheduledAutoTuneDetails =
        Prelude.Nothing
    }

-- | Undocumented member.
autoTuneDetails_scheduledAutoTuneDetails :: Lens.Lens' AutoTuneDetails (Prelude.Maybe ScheduledAutoTuneDetails)
autoTuneDetails_scheduledAutoTuneDetails = Lens.lens (\AutoTuneDetails' {scheduledAutoTuneDetails} -> scheduledAutoTuneDetails) (\s@AutoTuneDetails' {} a -> s {scheduledAutoTuneDetails = a} :: AutoTuneDetails)

instance Prelude.FromJSON AutoTuneDetails where
  parseJSON =
    Prelude.withObject
      "AutoTuneDetails"
      ( \x ->
          AutoTuneDetails'
            Prelude.<$> (x Prelude..:? "ScheduledAutoTuneDetails")
      )

instance Prelude.Hashable AutoTuneDetails

instance Prelude.NFData AutoTuneDetails
