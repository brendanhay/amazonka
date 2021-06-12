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
-- Module      : Network.AWS.MediaConvert.Types.TeletextPageType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.TeletextPageType
  ( TeletextPageType
      ( ..,
        TeletextPageType_PAGE_TYPE_ADDL_INFO,
        TeletextPageType_PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE,
        TeletextPageType_PAGE_TYPE_INITIAL,
        TeletextPageType_PAGE_TYPE_PROGRAM_SCHEDULE,
        TeletextPageType_PAGE_TYPE_SUBTITLE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
newtype TeletextPageType = TeletextPageType'
  { fromTeletextPageType ::
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

pattern TeletextPageType_PAGE_TYPE_ADDL_INFO :: TeletextPageType
pattern TeletextPageType_PAGE_TYPE_ADDL_INFO = TeletextPageType' "PAGE_TYPE_ADDL_INFO"

pattern TeletextPageType_PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE :: TeletextPageType
pattern TeletextPageType_PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE = TeletextPageType' "PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE"

pattern TeletextPageType_PAGE_TYPE_INITIAL :: TeletextPageType
pattern TeletextPageType_PAGE_TYPE_INITIAL = TeletextPageType' "PAGE_TYPE_INITIAL"

pattern TeletextPageType_PAGE_TYPE_PROGRAM_SCHEDULE :: TeletextPageType
pattern TeletextPageType_PAGE_TYPE_PROGRAM_SCHEDULE = TeletextPageType' "PAGE_TYPE_PROGRAM_SCHEDULE"

pattern TeletextPageType_PAGE_TYPE_SUBTITLE :: TeletextPageType
pattern TeletextPageType_PAGE_TYPE_SUBTITLE = TeletextPageType' "PAGE_TYPE_SUBTITLE"

{-# COMPLETE
  TeletextPageType_PAGE_TYPE_ADDL_INFO,
  TeletextPageType_PAGE_TYPE_HEARING_IMPAIRED_SUBTITLE,
  TeletextPageType_PAGE_TYPE_INITIAL,
  TeletextPageType_PAGE_TYPE_PROGRAM_SCHEDULE,
  TeletextPageType_PAGE_TYPE_SUBTITLE,
  TeletextPageType'
  #-}
