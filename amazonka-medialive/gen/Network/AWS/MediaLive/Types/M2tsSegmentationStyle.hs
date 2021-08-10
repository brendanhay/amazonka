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
-- Module      : Network.AWS.MediaLive.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsSegmentationStyle
  ( M2tsSegmentationStyle
      ( ..,
        M2tsSegmentationStyle_MAINTAIN_CADENCE,
        M2tsSegmentationStyle_RESET_CADENCE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | M2ts Segmentation Style
newtype M2tsSegmentationStyle = M2tsSegmentationStyle'
  { fromM2tsSegmentationStyle ::
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

pattern M2tsSegmentationStyle_MAINTAIN_CADENCE :: M2tsSegmentationStyle
pattern M2tsSegmentationStyle_MAINTAIN_CADENCE = M2tsSegmentationStyle' "MAINTAIN_CADENCE"

pattern M2tsSegmentationStyle_RESET_CADENCE :: M2tsSegmentationStyle
pattern M2tsSegmentationStyle_RESET_CADENCE = M2tsSegmentationStyle' "RESET_CADENCE"

{-# COMPLETE
  M2tsSegmentationStyle_MAINTAIN_CADENCE,
  M2tsSegmentationStyle_RESET_CADENCE,
  M2tsSegmentationStyle'
  #-}
