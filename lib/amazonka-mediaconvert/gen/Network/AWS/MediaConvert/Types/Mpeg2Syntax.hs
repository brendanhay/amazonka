{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.Mpeg2Syntax
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Mpeg2Syntax
  ( Mpeg2Syntax
      ( Mpeg2Syntax',
        Mpeg2SyntaxDefault,
        Mpeg2SyntaxD10,
        fromMpeg2Syntax
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specify whether this output's video uses the D10 syntax. Keep the default value to  not use the syntax. Related settings: When you choose D10 (D_10) for your MXF  profile (profile), you must also set this value to to D10 (D_10).
newtype Mpeg2Syntax = Mpeg2Syntax' {fromMpeg2Syntax :: Core.Text}
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

pattern Mpeg2SyntaxDefault :: Mpeg2Syntax
pattern Mpeg2SyntaxDefault = Mpeg2Syntax' "DEFAULT"

pattern Mpeg2SyntaxD10 :: Mpeg2Syntax
pattern Mpeg2SyntaxD10 = Mpeg2Syntax' "D_10"

{-# COMPLETE
  Mpeg2SyntaxDefault,
  Mpeg2SyntaxD10,
  Mpeg2Syntax'
  #-}
