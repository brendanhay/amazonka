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
-- Module      : Amazonka.SecurityLake.Types.AwsLogSourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AwsLogSourceType
  ( AwsLogSourceType
      ( ..,
        AwsLogSourceType_CLOUD_TRAIL,
        AwsLogSourceType_ROUTE53,
        AwsLogSourceType_SH_FINDINGS,
        AwsLogSourceType_VPC_FLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AwsLogSourceType = AwsLogSourceType'
  { fromAwsLogSourceType ::
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

pattern AwsLogSourceType_CLOUD_TRAIL :: AwsLogSourceType
pattern AwsLogSourceType_CLOUD_TRAIL = AwsLogSourceType' "CLOUD_TRAIL"

pattern AwsLogSourceType_ROUTE53 :: AwsLogSourceType
pattern AwsLogSourceType_ROUTE53 = AwsLogSourceType' "ROUTE53"

pattern AwsLogSourceType_SH_FINDINGS :: AwsLogSourceType
pattern AwsLogSourceType_SH_FINDINGS = AwsLogSourceType' "SH_FINDINGS"

pattern AwsLogSourceType_VPC_FLOW :: AwsLogSourceType
pattern AwsLogSourceType_VPC_FLOW = AwsLogSourceType' "VPC_FLOW"

{-# COMPLETE
  AwsLogSourceType_CLOUD_TRAIL,
  AwsLogSourceType_ROUTE53,
  AwsLogSourceType_SH_FINDINGS,
  AwsLogSourceType_VPC_FLOW,
  AwsLogSourceType'
  #-}
