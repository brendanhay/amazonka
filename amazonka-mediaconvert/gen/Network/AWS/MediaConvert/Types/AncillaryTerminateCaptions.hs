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
-- Module      : Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.AncillaryTerminateCaptions
  ( AncillaryTerminateCaptions
      ( ..,
        AncillaryTerminateCaptions_DISABLED,
        AncillaryTerminateCaptions_END_OF_INPUT
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | By default, the service terminates any unterminated captions at the end
-- of each input. If you want the caption to continue onto your next input,
-- disable this setting.
newtype AncillaryTerminateCaptions = AncillaryTerminateCaptions'
  { fromAncillaryTerminateCaptions ::
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

pattern AncillaryTerminateCaptions_DISABLED :: AncillaryTerminateCaptions
pattern AncillaryTerminateCaptions_DISABLED = AncillaryTerminateCaptions' "DISABLED"

pattern AncillaryTerminateCaptions_END_OF_INPUT :: AncillaryTerminateCaptions
pattern AncillaryTerminateCaptions_END_OF_INPUT = AncillaryTerminateCaptions' "END_OF_INPUT"

{-# COMPLETE
  AncillaryTerminateCaptions_DISABLED,
  AncillaryTerminateCaptions_END_OF_INPUT,
  AncillaryTerminateCaptions'
  #-}
