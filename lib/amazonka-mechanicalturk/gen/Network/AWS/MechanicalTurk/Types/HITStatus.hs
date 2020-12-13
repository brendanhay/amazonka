{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.HITStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.HITStatus
  ( HITStatus
      ( HITStatus',
        Assignable,
        Unassignable,
        Reviewable,
        Reviewing,
        Disposed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype HITStatus = HITStatus' Lude.Text
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

pattern Assignable :: HITStatus
pattern Assignable = HITStatus' "Assignable"

pattern Unassignable :: HITStatus
pattern Unassignable = HITStatus' "Unassignable"

pattern Reviewable :: HITStatus
pattern Reviewable = HITStatus' "Reviewable"

pattern Reviewing :: HITStatus
pattern Reviewing = HITStatus' "Reviewing"

pattern Disposed :: HITStatus
pattern Disposed = HITStatus' "Disposed"

{-# COMPLETE
  Assignable,
  Unassignable,
  Reviewable,
  Reviewing,
  Disposed,
  HITStatus'
  #-}
