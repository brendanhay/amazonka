{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.QualityFilter
  ( QualityFilter
    ( QualityFilter'
    , QualityFilterNone
    , QualityFilterAuto
    , QualityFilterLow
    , QualityFilterMedium
    , QualityFilterHigh
    , fromQualityFilter
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype QualityFilter = QualityFilter'{fromQualityFilter ::
                                       Core.Text}
                          deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                          Core.Generic)
                          deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                            Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                            Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                            Core.FromText, Core.ToByteString, Core.ToQuery,
                                            Core.ToHeader)

pattern QualityFilterNone :: QualityFilter
pattern QualityFilterNone = QualityFilter' "NONE"

pattern QualityFilterAuto :: QualityFilter
pattern QualityFilterAuto = QualityFilter' "AUTO"

pattern QualityFilterLow :: QualityFilter
pattern QualityFilterLow = QualityFilter' "LOW"

pattern QualityFilterMedium :: QualityFilter
pattern QualityFilterMedium = QualityFilter' "MEDIUM"

pattern QualityFilterHigh :: QualityFilter
pattern QualityFilterHigh = QualityFilter' "HIGH"

{-# COMPLETE 
  QualityFilterNone,

  QualityFilterAuto,

  QualityFilterLow,

  QualityFilterMedium,

  QualityFilterHigh,
  QualityFilter'
  #-}
