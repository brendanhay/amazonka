{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.EventSourcePosition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.EventSourcePosition
  ( EventSourcePosition
      ( EventSourcePosition',
        AtTimestamp,
        Latest,
        TrimHorizon
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EventSourcePosition = EventSourcePosition' Lude.Text
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

pattern AtTimestamp :: EventSourcePosition
pattern AtTimestamp = EventSourcePosition' "AT_TIMESTAMP"

pattern Latest :: EventSourcePosition
pattern Latest = EventSourcePosition' "LATEST"

pattern TrimHorizon :: EventSourcePosition
pattern TrimHorizon = EventSourcePosition' "TRIM_HORIZON"

{-# COMPLETE
  AtTimestamp,
  Latest,
  TrimHorizon,
  EventSourcePosition'
  #-}
