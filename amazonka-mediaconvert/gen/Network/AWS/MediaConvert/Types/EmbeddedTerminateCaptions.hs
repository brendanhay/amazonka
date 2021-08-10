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
-- Module      : Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.EmbeddedTerminateCaptions
  ( EmbeddedTerminateCaptions
      ( ..,
        EmbeddedTerminateCaptions_DISABLED,
        EmbeddedTerminateCaptions_END_OF_INPUT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
newtype EmbeddedTerminateCaptions = EmbeddedTerminateCaptions'
  { fromEmbeddedTerminateCaptions ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
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

pattern EmbeddedTerminateCaptions_DISABLED :: EmbeddedTerminateCaptions
pattern EmbeddedTerminateCaptions_DISABLED = EmbeddedTerminateCaptions' "DISABLED"

pattern EmbeddedTerminateCaptions_END_OF_INPUT :: EmbeddedTerminateCaptions
pattern EmbeddedTerminateCaptions_END_OF_INPUT = EmbeddedTerminateCaptions' "END_OF_INPUT"

{-# COMPLETE
  EmbeddedTerminateCaptions_DISABLED,
  EmbeddedTerminateCaptions_END_OF_INPUT,
  EmbeddedTerminateCaptions'
  #-}
