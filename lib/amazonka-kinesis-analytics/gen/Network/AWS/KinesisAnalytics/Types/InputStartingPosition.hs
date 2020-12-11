-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputStartingPosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputStartingPosition
  ( InputStartingPosition
      ( InputStartingPosition',
        LastStoppedPoint,
        Now,
        TrimHorizon
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InputStartingPosition = InputStartingPosition' Lude.Text
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

pattern LastStoppedPoint :: InputStartingPosition
pattern LastStoppedPoint = InputStartingPosition' "LAST_STOPPED_POINT"

pattern Now :: InputStartingPosition
pattern Now = InputStartingPosition' "NOW"

pattern TrimHorizon :: InputStartingPosition
pattern TrimHorizon = InputStartingPosition' "TRIM_HORIZON"

{-# COMPLETE
  LastStoppedPoint,
  Now,
  TrimHorizon,
  InputStartingPosition'
  #-}
