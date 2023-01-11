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
-- Module      : Amazonka.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ReportGroupSortByType
  ( ReportGroupSortByType
      ( ..,
        ReportGroupSortByType_CREATED_TIME,
        ReportGroupSortByType_LAST_MODIFIED_TIME,
        ReportGroupSortByType_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportGroupSortByType = ReportGroupSortByType'
  { fromReportGroupSortByType ::
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

pattern ReportGroupSortByType_CREATED_TIME :: ReportGroupSortByType
pattern ReportGroupSortByType_CREATED_TIME = ReportGroupSortByType' "CREATED_TIME"

pattern ReportGroupSortByType_LAST_MODIFIED_TIME :: ReportGroupSortByType
pattern ReportGroupSortByType_LAST_MODIFIED_TIME = ReportGroupSortByType' "LAST_MODIFIED_TIME"

pattern ReportGroupSortByType_NAME :: ReportGroupSortByType
pattern ReportGroupSortByType_NAME = ReportGroupSortByType' "NAME"

{-# COMPLETE
  ReportGroupSortByType_CREATED_TIME,
  ReportGroupSortByType_LAST_MODIFIED_TIME,
  ReportGroupSortByType_NAME,
  ReportGroupSortByType'
  #-}
