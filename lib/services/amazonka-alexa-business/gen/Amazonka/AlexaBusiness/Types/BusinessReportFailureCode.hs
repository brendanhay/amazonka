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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportFailureCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportFailureCode
  ( BusinessReportFailureCode
      ( ..,
        BusinessReportFailureCode_ACCESS_DENIED,
        BusinessReportFailureCode_INTERNAL_FAILURE,
        BusinessReportFailureCode_NO_SUCH_BUCKET
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype BusinessReportFailureCode = BusinessReportFailureCode'
  { fromBusinessReportFailureCode ::
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

pattern BusinessReportFailureCode_ACCESS_DENIED :: BusinessReportFailureCode
pattern BusinessReportFailureCode_ACCESS_DENIED = BusinessReportFailureCode' "ACCESS_DENIED"

pattern BusinessReportFailureCode_INTERNAL_FAILURE :: BusinessReportFailureCode
pattern BusinessReportFailureCode_INTERNAL_FAILURE = BusinessReportFailureCode' "INTERNAL_FAILURE"

pattern BusinessReportFailureCode_NO_SUCH_BUCKET :: BusinessReportFailureCode
pattern BusinessReportFailureCode_NO_SUCH_BUCKET = BusinessReportFailureCode' "NO_SUCH_BUCKET"

{-# COMPLETE
  BusinessReportFailureCode_ACCESS_DENIED,
  BusinessReportFailureCode_INTERNAL_FAILURE,
  BusinessReportFailureCode_NO_SUCH_BUCKET,
  BusinessReportFailureCode'
  #-}
