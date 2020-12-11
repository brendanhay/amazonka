-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.TargetState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.TargetState
  ( TargetState
      ( TargetState',
        TSAvailable,
        TSRegistering,
        TSUnavailable
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TargetState = TargetState' Lude.Text
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

pattern TSAvailable :: TargetState
pattern TSAvailable = TargetState' "AVAILABLE"

pattern TSRegistering :: TargetState
pattern TSRegistering = TargetState' "REGISTERING"

pattern TSUnavailable :: TargetState
pattern TSUnavailable = TargetState' "UNAVAILABLE"

{-# COMPLETE
  TSAvailable,
  TSRegistering,
  TSUnavailable,
  TargetState'
  #-}
