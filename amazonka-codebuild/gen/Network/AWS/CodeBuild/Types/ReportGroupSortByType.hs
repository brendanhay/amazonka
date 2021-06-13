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
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupSortByType
  ( ReportGroupSortByType
      ( ..,
        ReportGroupSortByType_CREATED_TIME,
        ReportGroupSortByType_LAST_MODIFIED_TIME,
        ReportGroupSortByType_NAME
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ReportGroupSortByType = ReportGroupSortByType'
  { fromReportGroupSortByType ::
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
