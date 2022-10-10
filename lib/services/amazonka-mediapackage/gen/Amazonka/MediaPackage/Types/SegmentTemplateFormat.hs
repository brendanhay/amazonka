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
-- Module      : Amazonka.MediaPackage.Types.SegmentTemplateFormat
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.SegmentTemplateFormat
  ( SegmentTemplateFormat
      ( ..,
        SegmentTemplateFormat_NUMBER_WITH_DURATION,
        SegmentTemplateFormat_NUMBER_WITH_TIMELINE,
        SegmentTemplateFormat_TIME_WITH_TIMELINE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SegmentTemplateFormat = SegmentTemplateFormat'
  { fromSegmentTemplateFormat ::
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
