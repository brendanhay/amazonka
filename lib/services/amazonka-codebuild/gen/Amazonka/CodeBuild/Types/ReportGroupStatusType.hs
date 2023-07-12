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
-- Module      : Amazonka.CodeBuild.Types.ReportGroupStatusType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportGroupStatusType
  ( ReportGroupStatusType
      ( ..,
        ReportGroupStatusType_ACTIVE,
        ReportGroupStatusType_DELETING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportGroupStatusType = ReportGroupStatusType'
  { fromReportGroupStatusType ::
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

pattern ReportGroupStatusType_ACTIVE :: ReportGroupStatusType
pattern ReportGroupStatusType_ACTIVE = ReportGroupStatusType' "ACTIVE"

pattern ReportGroupStatusType_DELETING :: ReportGroupStatusType
pattern ReportGroupStatusType_DELETING = ReportGroupStatusType' "DELETING"

{-# COMPLETE
  ReportGroupStatusType_ACTIVE,
  ReportGroupStatusType_DELETING,
  ReportGroupStatusType'
  #-}
