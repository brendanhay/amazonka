{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.EmbeddedConvert608To708
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.EmbeddedConvert608To708
  ( EmbeddedConvert608To708
      ( EmbeddedConvert608To708',
        ECTDisabled,
        ECTUpconvert
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Embedded Convert608 To708
newtype EmbeddedConvert608To708 = EmbeddedConvert608To708' Lude.Text
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

pattern ECTDisabled :: EmbeddedConvert608To708
pattern ECTDisabled = EmbeddedConvert608To708' "DISABLED"

pattern ECTUpconvert :: EmbeddedConvert608To708
pattern ECTUpconvert = EmbeddedConvert608To708' "UPCONVERT"

{-# COMPLETE
  ECTDisabled,
  ECTUpconvert,
  EmbeddedConvert608To708'
  #-}
