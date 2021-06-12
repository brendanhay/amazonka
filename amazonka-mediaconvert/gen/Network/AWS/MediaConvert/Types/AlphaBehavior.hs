{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.AlphaBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AlphaBehavior
  ( AlphaBehavior
      ( ..,
        AlphaBehavior_DISCARD,
        AlphaBehavior_REMAP_TO_LUMA
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Ignore this setting unless this input is a QuickTime animation with an
-- alpha channel. Use this setting to create separate Key and Fill outputs.
-- In each output, specify which part of the input MediaConvert uses. Leave
-- this setting at the default value DISCARD to delete the alpha channel
-- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
-- map the alpha channel to the luma channel of your outputs.
newtype AlphaBehavior = AlphaBehavior'
  { fromAlphaBehavior ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern AlphaBehavior_DISCARD :: AlphaBehavior
pattern AlphaBehavior_DISCARD = AlphaBehavior' "DISCARD"

pattern AlphaBehavior_REMAP_TO_LUMA :: AlphaBehavior
pattern AlphaBehavior_REMAP_TO_LUMA = AlphaBehavior' "REMAP_TO_LUMA"

{-# COMPLETE
  AlphaBehavior_DISCARD,
  AlphaBehavior_REMAP_TO_LUMA,
  AlphaBehavior'
  #-}
