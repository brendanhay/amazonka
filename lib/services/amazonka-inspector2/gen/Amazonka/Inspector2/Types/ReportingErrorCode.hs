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
-- Module      : Amazonka.Inspector2.Types.ReportingErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ReportingErrorCode
  ( ReportingErrorCode
      ( ..,
        ReportingErrorCode_BUCKET_NOT_FOUND,
        ReportingErrorCode_INCOMPATIBLE_BUCKET_REGION,
        ReportingErrorCode_INTERNAL_ERROR,
        ReportingErrorCode_INVALID_PERMISSIONS,
        ReportingErrorCode_MALFORMED_KMS_KEY,
        ReportingErrorCode_NO_FINDINGS_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ReportingErrorCode = ReportingErrorCode'
  { fromReportingErrorCode ::
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

pattern ReportingErrorCode_BUCKET_NOT_FOUND :: ReportingErrorCode
pattern ReportingErrorCode_BUCKET_NOT_FOUND = ReportingErrorCode' "BUCKET_NOT_FOUND"

pattern ReportingErrorCode_INCOMPATIBLE_BUCKET_REGION :: ReportingErrorCode
pattern ReportingErrorCode_INCOMPATIBLE_BUCKET_REGION = ReportingErrorCode' "INCOMPATIBLE_BUCKET_REGION"

pattern ReportingErrorCode_INTERNAL_ERROR :: ReportingErrorCode
pattern ReportingErrorCode_INTERNAL_ERROR = ReportingErrorCode' "INTERNAL_ERROR"

pattern ReportingErrorCode_INVALID_PERMISSIONS :: ReportingErrorCode
pattern ReportingErrorCode_INVALID_PERMISSIONS = ReportingErrorCode' "INVALID_PERMISSIONS"

pattern ReportingErrorCode_MALFORMED_KMS_KEY :: ReportingErrorCode
pattern ReportingErrorCode_MALFORMED_KMS_KEY = ReportingErrorCode' "MALFORMED_KMS_KEY"

pattern ReportingErrorCode_NO_FINDINGS_FOUND :: ReportingErrorCode
pattern ReportingErrorCode_NO_FINDINGS_FOUND = ReportingErrorCode' "NO_FINDINGS_FOUND"

{-# COMPLETE
  ReportingErrorCode_BUCKET_NOT_FOUND,
  ReportingErrorCode_INCOMPATIBLE_BUCKET_REGION,
  ReportingErrorCode_INTERNAL_ERROR,
  ReportingErrorCode_INVALID_PERMISSIONS,
  ReportingErrorCode_MALFORMED_KMS_KEY,
  ReportingErrorCode_NO_FINDINGS_FOUND,
  ReportingErrorCode'
  #-}
