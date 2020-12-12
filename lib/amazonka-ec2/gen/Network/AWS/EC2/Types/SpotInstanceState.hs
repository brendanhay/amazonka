{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotInstanceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotInstanceState
  ( SpotInstanceState
      ( SpotInstanceState',
        SISActive,
        SISCancelled,
        SISClosed,
        SISFailed,
        SISOpen
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SpotInstanceState = SpotInstanceState' Lude.Text
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

pattern SISActive :: SpotInstanceState
pattern SISActive = SpotInstanceState' "active"

pattern SISCancelled :: SpotInstanceState
pattern SISCancelled = SpotInstanceState' "cancelled"

pattern SISClosed :: SpotInstanceState
pattern SISClosed = SpotInstanceState' "closed"

pattern SISFailed :: SpotInstanceState
pattern SISFailed = SpotInstanceState' "failed"

pattern SISOpen :: SpotInstanceState
pattern SISOpen = SpotInstanceState' "open"

{-# COMPLETE
  SISActive,
  SISCancelled,
  SISClosed,
  SISFailed,
  SISOpen,
  SpotInstanceState'
  #-}
