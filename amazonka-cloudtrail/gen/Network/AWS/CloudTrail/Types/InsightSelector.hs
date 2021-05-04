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
-- Module      : Network.AWS.CloudTrail.Types.InsightSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudTrail.Types.InsightSelector where

import Network.AWS.CloudTrail.Types.InsightType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A JSON string that contains a list of insight types that are logged on a
-- trail.
--
-- /See:/ 'newInsightSelector' smart constructor.
data InsightSelector = InsightSelector'
  { -- | The type of insights to log on a trail. In this release, only
    -- @ApiCallRateInsight@ is supported as an insight type.
    insightType :: Prelude.Maybe InsightType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InsightSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightType', 'insightSelector_insightType' - The type of insights to log on a trail. In this release, only
-- @ApiCallRateInsight@ is supported as an insight type.
newInsightSelector ::
  InsightSelector
newInsightSelector =
  InsightSelector' {insightType = Prelude.Nothing}

-- | The type of insights to log on a trail. In this release, only
-- @ApiCallRateInsight@ is supported as an insight type.
insightSelector_insightType :: Lens.Lens' InsightSelector (Prelude.Maybe InsightType)
insightSelector_insightType = Lens.lens (\InsightSelector' {insightType} -> insightType) (\s@InsightSelector' {} a -> s {insightType = a} :: InsightSelector)

instance Prelude.FromJSON InsightSelector where
  parseJSON =
    Prelude.withObject
      "InsightSelector"
      ( \x ->
          InsightSelector'
            Prelude.<$> (x Prelude..:? "InsightType")
      )

instance Prelude.Hashable InsightSelector

instance Prelude.NFData InsightSelector

instance Prelude.ToJSON InsightSelector where
  toJSON InsightSelector' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("InsightType" Prelude..=) Prelude.<$> insightType]
      )
