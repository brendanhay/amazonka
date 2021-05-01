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

import qualified Network.AWS.Prelude as Prelude

newtype SegmentTemplateFormat = SegmentTemplateFormat'
  { fromSegmentTemplateFormat ::
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
