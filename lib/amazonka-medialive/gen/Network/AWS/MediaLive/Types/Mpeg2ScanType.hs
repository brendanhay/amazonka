{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.Mpeg2ScanType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mpeg2ScanType
  ( Mpeg2ScanType
      ( Mpeg2ScanType',
        MSTInterlaced,
        MSTProgressive
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Mpeg2 Scan Type
newtype Mpeg2ScanType = Mpeg2ScanType' Lude.Text
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

pattern MSTInterlaced :: Mpeg2ScanType
pattern MSTInterlaced = Mpeg2ScanType' "INTERLACED"

pattern MSTProgressive :: Mpeg2ScanType
pattern MSTProgressive = Mpeg2ScanType' "PROGRESSIVE"

{-# COMPLETE
  MSTInterlaced,
  MSTProgressive,
  Mpeg2ScanType'
  #-}
