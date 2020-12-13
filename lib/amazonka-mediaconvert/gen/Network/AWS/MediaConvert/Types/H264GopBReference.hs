{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264GopBReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264GopBReference
  ( H264GopBReference
      ( H264GopBReference',
        HGBRfDisabled,
        HGBRfEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | If enable, use reference B frames for GOP structures that have B frames > 1.
newtype H264GopBReference = H264GopBReference' Lude.Text
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

pattern HGBRfDisabled :: H264GopBReference
pattern HGBRfDisabled = H264GopBReference' "DISABLED"

pattern HGBRfEnabled :: H264GopBReference
pattern HGBRfEnabled = H264GopBReference' "ENABLED"

{-# COMPLETE
  HGBRfDisabled,
  HGBRfEnabled,
  H264GopBReference'
  #-}
