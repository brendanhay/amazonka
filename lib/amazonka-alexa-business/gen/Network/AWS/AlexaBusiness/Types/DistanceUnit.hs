-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DistanceUnit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DistanceUnit
  ( DistanceUnit
      ( DistanceUnit',
        Imperial,
        Metric
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DistanceUnit = DistanceUnit' Lude.Text
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

pattern Imperial :: DistanceUnit
pattern Imperial = DistanceUnit' "IMPERIAL"

pattern Metric :: DistanceUnit
pattern Metric = DistanceUnit' "METRIC"

{-# COMPLETE
  Imperial,
  Metric,
  DistanceUnit'
  #-}
