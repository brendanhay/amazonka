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
-- Module      : Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.M2tsSegmentationStyle
  ( M2tsSegmentationStyle
      ( ..,
        M2tsSegmentationStyle_MAINTAIN_CADENCE,
        M2tsSegmentationStyle_RESET_CADENCE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The segmentation style parameter controls how segmentation markers are
-- inserted into the transport stream. With avails, it is possible that
-- segments may be truncated, which can influence where future segmentation
-- markers are inserted. When a segmentation style of \"reset_cadence\" is
-- selected and a segment is truncated due to an avail, we will reset the
-- segmentation cadence. This means the subsequent segment will have a
-- duration of of $segmentation_time seconds. When a segmentation style of
-- \"maintain_cadence\" is selected and a segment is truncated due to an
-- avail, we will not reset the segmentation cadence. This means the
-- subsequent segment will likely be truncated as well. However, all
-- segments after that will have a duration of $segmentation_time seconds.
-- Note that EBP lookahead is a slight exception to this rule.
newtype M2tsSegmentationStyle = M2tsSegmentationStyle'
  { fromM2tsSegmentationStyle ::
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

pattern M2tsSegmentationStyle_MAINTAIN_CADENCE :: M2tsSegmentationStyle
pattern M2tsSegmentationStyle_MAINTAIN_CADENCE = M2tsSegmentationStyle' "MAINTAIN_CADENCE"

pattern M2tsSegmentationStyle_RESET_CADENCE :: M2tsSegmentationStyle
pattern M2tsSegmentationStyle_RESET_CADENCE = M2tsSegmentationStyle' "RESET_CADENCE"

{-# COMPLETE
  M2tsSegmentationStyle_MAINTAIN_CADENCE,
  M2tsSegmentationStyle_RESET_CADENCE,
  M2tsSegmentationStyle'
  #-}
