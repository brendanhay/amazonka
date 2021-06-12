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
-- Module      : Network.AWS.MediaPackage.Types.SegmentTemplateFormat
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.SegmentTemplateFormat
  ( SegmentTemplateFormat
      ( ..,
        SegmentTemplateFormat_NUMBER_WITH_DURATION,
        SegmentTemplateFormat_NUMBER_WITH_TIMELINE,
        SegmentTemplateFormat_TIME_WITH_TIMELINE
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SegmentTemplateFormat = SegmentTemplateFormat'
  { fromSegmentTemplateFormat ::
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

pattern SegmentTemplateFormat_NUMBER_WITH_DURATION :: SegmentTemplateFormat
pattern SegmentTemplateFormat_NUMBER_WITH_DURATION = SegmentTemplateFormat' "NUMBER_WITH_DURATION"

pattern SegmentTemplateFormat_NUMBER_WITH_TIMELINE :: SegmentTemplateFormat
pattern SegmentTemplateFormat_NUMBER_WITH_TIMELINE = SegmentTemplateFormat' "NUMBER_WITH_TIMELINE"

pattern SegmentTemplateFormat_TIME_WITH_TIMELINE :: SegmentTemplateFormat
pattern SegmentTemplateFormat_TIME_WITH_TIMELINE = SegmentTemplateFormat' "TIME_WITH_TIMELINE"

{-# COMPLETE
  SegmentTemplateFormat_NUMBER_WITH_DURATION,
  SegmentTemplateFormat_NUMBER_WITH_TIMELINE,
  SegmentTemplateFormat_TIME_WITH_TIMELINE,
  SegmentTemplateFormat'
  #-}
