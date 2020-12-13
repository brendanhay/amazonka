{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Seconds,
        Minutes,
        Hours,
        Days
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

pattern Seconds :: TimeUnitsType
pattern Seconds = TimeUnitsType' "seconds"

pattern Minutes :: TimeUnitsType
pattern Minutes = TimeUnitsType' "minutes"

pattern Hours :: TimeUnitsType
pattern Hours = TimeUnitsType' "hours"

pattern Days :: TimeUnitsType
pattern Days = TimeUnitsType' "days"

{-# COMPLETE
  Seconds,
  Minutes,
  Hours,
  Days,
  TimeUnitsType'
  #-}
