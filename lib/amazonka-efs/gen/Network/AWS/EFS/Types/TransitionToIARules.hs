{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.Types.TransitionToIARules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EFS.Types.TransitionToIARules
  ( TransitionToIARules
      ( TransitionToIARules',
        After14Days,
        After30Days,
        After60Days,
        After7Days,
        After90Days
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TransitionToIARules = TransitionToIARules' Lude.Text
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

pattern After14Days :: TransitionToIARules
pattern After14Days = TransitionToIARules' "AFTER_14_DAYS"

pattern After30Days :: TransitionToIARules
pattern After30Days = TransitionToIARules' "AFTER_30_DAYS"

pattern After60Days :: TransitionToIARules
pattern After60Days = TransitionToIARules' "AFTER_60_DAYS"

pattern After7Days :: TransitionToIARules
pattern After7Days = TransitionToIARules' "AFTER_7_DAYS"

pattern After90Days :: TransitionToIARules
pattern After90Days = TransitionToIARules' "AFTER_90_DAYS"

{-# COMPLETE
  After14Days,
  After30Days,
  After60Days,
  After7Days,
  After90Days,
  TransitionToIARules'
  #-}
