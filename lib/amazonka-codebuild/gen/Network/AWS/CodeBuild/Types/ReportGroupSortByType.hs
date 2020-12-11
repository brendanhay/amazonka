-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportGroupSortByType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportGroupSortByType
  ( ReportGroupSortByType
      ( ReportGroupSortByType',
        RGSBTCreatedTime,
        RGSBTLastModifiedTime,
        RGSBTName
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportGroupSortByType = ReportGroupSortByType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern RGSBTCreatedTime :: ReportGroupSortByType
pattern RGSBTCreatedTime = ReportGroupSortByType' "CREATED_TIME"

pattern RGSBTLastModifiedTime :: ReportGroupSortByType
pattern RGSBTLastModifiedTime = ReportGroupSortByType' "LAST_MODIFIED_TIME"

pattern RGSBTName :: ReportGroupSortByType
pattern RGSBTName = ReportGroupSortByType' "NAME"

{-# COMPLETE
  RGSBTCreatedTime,
  RGSBTLastModifiedTime,
  RGSBTName,
  ReportGroupSortByType'
  #-}
