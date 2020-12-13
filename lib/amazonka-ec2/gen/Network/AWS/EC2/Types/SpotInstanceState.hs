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
        SISOpen,
        SISActive,
        SISClosed,
        SISCancelled,
        SISFailed
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

pattern SISOpen :: SpotInstanceState
pattern SISOpen = SpotInstanceState' "open"

pattern SISActive :: SpotInstanceState
pattern SISActive = SpotInstanceState' "active"

pattern SISClosed :: SpotInstanceState
pattern SISClosed = SpotInstanceState' "closed"

pattern SISCancelled :: SpotInstanceState
pattern SISCancelled = SpotInstanceState' "cancelled"

pattern SISFailed :: SpotInstanceState
pattern SISFailed = SpotInstanceState' "failed"

{-# COMPLETE
  SISOpen,
  SISActive,
  SISClosed,
  SISCancelled,
  SISFailed,
  SpotInstanceState'
  #-}
