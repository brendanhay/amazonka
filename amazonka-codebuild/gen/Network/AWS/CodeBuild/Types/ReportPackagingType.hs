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
-- Module      : Network.AWS.CodeBuild.Types.ReportPackagingType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ReportPackagingType
  ( ReportPackagingType
      ( ..,
        ReportPackagingType_NONE,
        ReportPackagingType_ZIP
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ReportPackagingType = ReportPackagingType'
  { fromReportPackagingType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ReportPackagingType_NONE :: ReportPackagingType
pattern ReportPackagingType_NONE = ReportPackagingType' "NONE"

pattern ReportPackagingType_ZIP :: ReportPackagingType
pattern ReportPackagingType_ZIP = ReportPackagingType' "ZIP"

{-# COMPLETE
  ReportPackagingType_NONE,
  ReportPackagingType_ZIP,
  ReportPackagingType'
  #-}
