{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H265GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265GopBReference
  ( H265GopBReference
      ( H265GopBReference',
        HGBRDisabled,
        HGBREnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H265GopBReference = H265GopBReference' Lude.Text
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

pattern HGBRDisabled :: H265GopBReference
pattern HGBRDisabled = H265GopBReference' "DISABLED"

pattern HGBREnabled :: H265GopBReference
pattern HGBREnabled = H265GopBReference' "ENABLED"

{-# COMPLETE
  HGBRDisabled,
  HGBREnabled,
  H265GopBReference'
  #-}
