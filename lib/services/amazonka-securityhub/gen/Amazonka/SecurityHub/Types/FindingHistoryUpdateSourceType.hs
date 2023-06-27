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
-- Module      : Amazonka.SecurityHub.Types.FindingHistoryUpdateSourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.FindingHistoryUpdateSourceType
  ( FindingHistoryUpdateSourceType
      ( ..,
        FindingHistoryUpdateSourceType_BATCH_IMPORT_FINDINGS,
        FindingHistoryUpdateSourceType_BATCH_UPDATE_FINDINGS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FindingHistoryUpdateSourceType = FindingHistoryUpdateSourceType'
  { fromFindingHistoryUpdateSourceType ::
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

pattern FindingHistoryUpdateSourceType_BATCH_IMPORT_FINDINGS :: FindingHistoryUpdateSourceType
pattern FindingHistoryUpdateSourceType_BATCH_IMPORT_FINDINGS = FindingHistoryUpdateSourceType' "BATCH_IMPORT_FINDINGS"

pattern FindingHistoryUpdateSourceType_BATCH_UPDATE_FINDINGS :: FindingHistoryUpdateSourceType
pattern FindingHistoryUpdateSourceType_BATCH_UPDATE_FINDINGS = FindingHistoryUpdateSourceType' "BATCH_UPDATE_FINDINGS"

{-# COMPLETE
  FindingHistoryUpdateSourceType_BATCH_IMPORT_FINDINGS,
  FindingHistoryUpdateSourceType_BATCH_UPDATE_FINDINGS,
  FindingHistoryUpdateSourceType'
  #-}
