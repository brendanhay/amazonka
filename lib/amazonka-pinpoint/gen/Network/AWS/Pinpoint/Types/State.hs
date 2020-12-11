-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.State
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.State
  ( State
      ( State',
        SActive,
        SCancelled,
        SClosed,
        SCompleted,
        SDraft
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype State = State' Lude.Text
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

pattern SActive :: State
pattern SActive = State' "ACTIVE"

pattern SCancelled :: State
pattern SCancelled = State' "CANCELLED"

pattern SClosed :: State
pattern SClosed = State' "CLOSED"

pattern SCompleted :: State
pattern SCompleted = State' "COMPLETED"

pattern SDraft :: State
pattern SDraft = State' "DRAFT"

{-# COMPLETE
  SActive,
  SCancelled,
  SClosed,
  SCompleted,
  SDraft,
  State'
  #-}
