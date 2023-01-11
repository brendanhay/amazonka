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
-- Module      : Amazonka.AccessAnalyzer.Types.JobErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.JobErrorCode
  ( JobErrorCode
      ( ..,
        JobErrorCode_AUTHORIZATION_ERROR,
        JobErrorCode_RESOURCE_NOT_FOUND_ERROR,
        JobErrorCode_SERVICE_ERROR,
        JobErrorCode_SERVICE_QUOTA_EXCEEDED_ERROR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JobErrorCode = JobErrorCode'
  { fromJobErrorCode ::
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

pattern JobErrorCode_AUTHORIZATION_ERROR :: JobErrorCode
pattern JobErrorCode_AUTHORIZATION_ERROR = JobErrorCode' "AUTHORIZATION_ERROR"

pattern JobErrorCode_RESOURCE_NOT_FOUND_ERROR :: JobErrorCode
pattern JobErrorCode_RESOURCE_NOT_FOUND_ERROR = JobErrorCode' "RESOURCE_NOT_FOUND_ERROR"

pattern JobErrorCode_SERVICE_ERROR :: JobErrorCode
pattern JobErrorCode_SERVICE_ERROR = JobErrorCode' "SERVICE_ERROR"

pattern JobErrorCode_SERVICE_QUOTA_EXCEEDED_ERROR :: JobErrorCode
pattern JobErrorCode_SERVICE_QUOTA_EXCEEDED_ERROR = JobErrorCode' "SERVICE_QUOTA_EXCEEDED_ERROR"

{-# COMPLETE
  JobErrorCode_AUTHORIZATION_ERROR,
  JobErrorCode_RESOURCE_NOT_FOUND_ERROR,
  JobErrorCode_SERVICE_ERROR,
  JobErrorCode_SERVICE_QUOTA_EXCEEDED_ERROR,
  JobErrorCode'
  #-}
