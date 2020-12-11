-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.QualityFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.QualityFilter
  ( QualityFilter
      ( QualityFilter',
        Auto,
        High,
        Low,
        Medium,
        None
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype QualityFilter = QualityFilter' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Auto :: QualityFilter
pattern Auto = QualityFilter' "AUTO"

pattern High :: QualityFilter
pattern High = QualityFilter' "HIGH"

pattern Low :: QualityFilter
pattern Low = QualityFilter' "LOW"

pattern Medium :: QualityFilter
pattern Medium = QualityFilter' "MEDIUM"

pattern None :: QualityFilter
pattern None = QualityFilter' "NONE"

{-# COMPLETE
  Auto,
  High,
  Low,
  Medium,
  None,
  QualityFilter'
  #-}
