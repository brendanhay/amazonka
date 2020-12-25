{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AlphaBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AlphaBehavior
  ( AlphaBehavior
      ( AlphaBehavior',
        AlphaBehaviorDiscard,
        AlphaBehaviorRemapToLuma,
        fromAlphaBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Ignore this setting unless this input is a QuickTime animation with an alpha channel. Use this setting to create separate Key and Fill outputs. In each output, specify which part of the input MediaConvert uses. Leave this setting at the default value DISCARD to delete the alpha channel and preserve the video. Set it to REMAP_TO_LUMA to delete the video and map the alpha channel to the luma channel of your outputs.
newtype AlphaBehavior = AlphaBehavior'
  { fromAlphaBehavior ::
      Core.Text
  }
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

pattern AlphaBehaviorDiscard :: AlphaBehavior
pattern AlphaBehaviorDiscard = AlphaBehavior' "DISCARD"

pattern AlphaBehaviorRemapToLuma :: AlphaBehavior
pattern AlphaBehaviorRemapToLuma = AlphaBehavior' "REMAP_TO_LUMA"

{-# COMPLETE
  AlphaBehaviorDiscard,
  AlphaBehaviorRemapToLuma,
  AlphaBehavior'
  #-}
