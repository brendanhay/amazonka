{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.ReportExportConfigType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportExportConfigType
  ( ReportExportConfigType
      ( ..,
        ReportExportConfigType_NO_EXPORT,
        ReportExportConfigType_S3
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ReportExportConfigType = ReportExportConfigType'
  { fromReportExportConfigType ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ReportExportConfigType_NO_EXPORT :: ReportExportConfigType
pattern ReportExportConfigType_NO_EXPORT = ReportExportConfigType' "NO_EXPORT"

pattern ReportExportConfigType_S3 :: ReportExportConfigType
pattern ReportExportConfigType_S3 = ReportExportConfigType' "S3"

{-# COMPLETE
  ReportExportConfigType_NO_EXPORT,
  ReportExportConfigType_S3,
  ReportExportConfigType'
  #-}
