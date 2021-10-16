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
-- Module      : Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MpdAccessibilityCaptionHints
  ( MpdAccessibilityCaptionHints
      ( ..,
        MpdAccessibilityCaptionHints_EXCLUDE,
        MpdAccessibilityCaptionHints_INCLUDE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Optional. Choose Include (INCLUDE) to have MediaConvert mark up your
-- DASH manifest with elements for embedded 608 captions. This markup
-- isn\'t generally required, but some video players require it to discover
-- and play embedded 608 captions. Keep the default value, Exclude
-- (EXCLUDE), to leave these elements out. When you enable this setting,
-- this is the markup that MediaConvert includes in your manifest:
newtype MpdAccessibilityCaptionHints = MpdAccessibilityCaptionHints'
  { fromMpdAccessibilityCaptionHints ::
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

pattern MpdAccessibilityCaptionHints_EXCLUDE :: MpdAccessibilityCaptionHints
pattern MpdAccessibilityCaptionHints_EXCLUDE = MpdAccessibilityCaptionHints' "EXCLUDE"

pattern MpdAccessibilityCaptionHints_INCLUDE :: MpdAccessibilityCaptionHints
pattern MpdAccessibilityCaptionHints_INCLUDE = MpdAccessibilityCaptionHints' "INCLUDE"

{-# COMPLETE
  MpdAccessibilityCaptionHints_EXCLUDE,
  MpdAccessibilityCaptionHints_INCLUDE,
  MpdAccessibilityCaptionHints'
  #-}
