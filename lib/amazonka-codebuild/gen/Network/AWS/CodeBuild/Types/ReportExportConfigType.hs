-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportExportConfigType
  ( ReportExportConfigType
      ( ReportExportConfigType',
        RECTNoExport,
        RECTS3
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ReportExportConfigType = ReportExportConfigType' Lude.Text
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

pattern RECTNoExport :: ReportExportConfigType
pattern RECTNoExport = ReportExportConfigType' "NO_EXPORT"

pattern RECTS3 :: ReportExportConfigType
pattern RECTS3 = ReportExportConfigType' "S3"

{-# COMPLETE
  RECTNoExport,
  RECTS3,
  ReportExportConfigType'
  #-}
