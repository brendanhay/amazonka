{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsForceTsVideoEbpOrder
  ( M2tsForceTsVideoEbpOrder
      ( M2tsForceTsVideoEbpOrder',
        MFTVEOForce,
        MFTVEODefault
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Keep the default value (DEFAULT) unless you know that your audio EBP markers are incorrectly appearing before your video EBP markers. To correct this problem, set this value to Force (FORCE).
newtype M2tsForceTsVideoEbpOrder = M2tsForceTsVideoEbpOrder' Lude.Text
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

pattern MFTVEOForce :: M2tsForceTsVideoEbpOrder
pattern MFTVEOForce = M2tsForceTsVideoEbpOrder' "FORCE"

pattern MFTVEODefault :: M2tsForceTsVideoEbpOrder
pattern MFTVEODefault = M2tsForceTsVideoEbpOrder' "DEFAULT"

{-# COMPLETE
  MFTVEOForce,
  MFTVEODefault,
  M2tsForceTsVideoEbpOrder'
  #-}
