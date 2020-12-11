-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsEsRateInPes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsEsRateInPes
  ( M2tsEsRateInPes
      ( M2tsEsRateInPes',
        MERIPExclude,
        MERIPInclude
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Es Rate In Pes
newtype M2tsEsRateInPes = M2tsEsRateInPes' Lude.Text
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

pattern MERIPExclude :: M2tsEsRateInPes
pattern MERIPExclude = M2tsEsRateInPes' "EXCLUDE"

pattern MERIPInclude :: M2tsEsRateInPes
pattern MERIPInclude = M2tsEsRateInPes' "INCLUDE"

{-# COMPLETE
  MERIPExclude,
  MERIPInclude,
  M2tsEsRateInPes'
  #-}
