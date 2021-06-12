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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportFailureCode
  ( BusinessReportFailureCode
      ( ..,
        BusinessReportFailureCode_ACCESS_DENIED,
        BusinessReportFailureCode_INTERNAL_FAILURE,
        BusinessReportFailureCode_NO_SUCH_BUCKET
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype BusinessReportFailureCode = BusinessReportFailureCode'
  { fromBusinessReportFailureCode ::
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
