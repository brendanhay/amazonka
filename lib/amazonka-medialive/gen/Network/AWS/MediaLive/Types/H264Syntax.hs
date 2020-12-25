{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H264Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264Syntax
  ( H264Syntax
      ( H264Syntax',
        H264SyntaxDefault,
        H264SyntaxRP2027,
        fromH264Syntax
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | H264 Syntax
newtype H264Syntax = H264Syntax' {fromH264Syntax :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern H264SyntaxDefault :: H264Syntax
pattern H264SyntaxDefault = H264Syntax' "DEFAULT"

pattern H264SyntaxRP2027 :: H264Syntax
pattern H264SyntaxRP2027 = H264Syntax' "RP2027"

{-# COMPLETE
  H264SyntaxDefault,
  H264SyntaxRP2027,
  H264Syntax'
  #-}
