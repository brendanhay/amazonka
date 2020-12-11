-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.TimeUnitsType
  ( TimeUnitsType
      ( TimeUnitsType',
        Days,
        Hours,
        Minutes,
        Seconds
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TimeUnitsType = TimeUnitsType' Lude.Text
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

pattern Days :: TimeUnitsType
pattern Days = TimeUnitsType' "days"

pattern Hours :: TimeUnitsType
pattern Hours = TimeUnitsType' "hours"

pattern Minutes :: TimeUnitsType
pattern Minutes = TimeUnitsType' "minutes"

pattern Seconds :: TimeUnitsType
pattern Seconds = TimeUnitsType' "seconds"

{-# COMPLETE
  Days,
  Hours,
  Minutes,
  Seconds,
  TimeUnitsType'
  #-}
