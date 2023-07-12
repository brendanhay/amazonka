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
-- Module      : Amazonka.MediaConvert.Types.TeletextPageType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TeletextPageType
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A page type as defined in the standard ETSI EN 300 468, Table 94
newtype TeletextPageType = TeletextPageType'
  { fromTeletextPageType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
