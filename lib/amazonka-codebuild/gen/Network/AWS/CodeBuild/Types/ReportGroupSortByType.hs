{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodeBuild.Types.ReportGroupSortByType
  ( ReportGroupSortByType
    ( ReportGroupSortByType'
    , ReportGroupSortByTypeName
    , ReportGroupSortByTypeCreatedTime
    , ReportGroupSortByTypeLastModifiedTime
    , fromReportGroupSortByType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ReportGroupSortByType = ReportGroupSortByType'{fromReportGroupSortByType
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern ReportGroupSortByTypeName :: ReportGroupSortByType
pattern ReportGroupSortByTypeName = ReportGroupSortByType' "NAME"

pattern ReportGroupSortByTypeCreatedTime :: ReportGroupSortByType
pattern ReportGroupSortByTypeCreatedTime = ReportGroupSortByType' "CREATED_TIME"

pattern ReportGroupSortByTypeLastModifiedTime :: ReportGroupSortByType
pattern ReportGroupSortByTypeLastModifiedTime = ReportGroupSortByType' "LAST_MODIFIED_TIME"

{-# COMPLETE 
  ReportGroupSortByTypeName,

  ReportGroupSortByTypeCreatedTime,

  ReportGroupSortByTypeLastModifiedTime,
  ReportGroupSortByType'
  #-}
