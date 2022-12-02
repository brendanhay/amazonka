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
-- Module      : Amazonka.CodeBuild.Types.ReportStatusType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportStatusType
  ( ReportStatusType
      ( ..,
        ReportStatusType_DELETING,
        ReportStatusType_FAILED,
        ReportStatusType_GENERATING,
        ReportStatusType_INCOMPLETE,
        ReportStatusType_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportStatusType = ReportStatusType'
  { fromReportStatusType ::
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

pattern ReportStatusType_DELETING :: ReportStatusType
pattern ReportStatusType_DELETING = ReportStatusType' "DELETING"

pattern ReportStatusType_FAILED :: ReportStatusType
pattern ReportStatusType_FAILED = ReportStatusType' "FAILED"

pattern ReportStatusType_GENERATING :: ReportStatusType
pattern ReportStatusType_GENERATING = ReportStatusType' "GENERATING"

pattern ReportStatusType_INCOMPLETE :: ReportStatusType
pattern ReportStatusType_INCOMPLETE = ReportStatusType' "INCOMPLETE"

pattern ReportStatusType_SUCCEEDED :: ReportStatusType
pattern ReportStatusType_SUCCEEDED = ReportStatusType' "SUCCEEDED"

{-# COMPLETE
  ReportStatusType_DELETING,
  ReportStatusType_FAILED,
  ReportStatusType_GENERATING,
  ReportStatusType_INCOMPLETE,
  ReportStatusType_SUCCEEDED,
  ReportStatusType'
  #-}
