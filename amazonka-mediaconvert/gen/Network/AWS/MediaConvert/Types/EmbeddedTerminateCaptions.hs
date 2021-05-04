{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
newtype EmbeddedTerminateCaptions = EmbeddedTerminateCaptions'
  { fromEmbeddedTerminateCaptions ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
