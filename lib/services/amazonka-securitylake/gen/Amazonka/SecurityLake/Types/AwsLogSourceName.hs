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
-- Module      : Amazonka.SecurityLake.Types.AwsLogSourceName
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.AwsLogSourceName
  ( AwsLogSourceName
      ( ..,
        AwsLogSourceName_CLOUD_TRAIL_MGMT,
        AwsLogSourceName_LAMBDA_EXECUTION,
        AwsLogSourceName_ROUTE53,
        AwsLogSourceName_S3_DATA,
        AwsLogSourceName_SH_FINDINGS,
        AwsLogSourceName_VPC_FLOW
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AwsLogSourceName = AwsLogSourceName'
  { fromAwsLogSourceName ::
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

pattern AwsLogSourceName_CLOUD_TRAIL_MGMT :: AwsLogSourceName
pattern AwsLogSourceName_CLOUD_TRAIL_MGMT = AwsLogSourceName' "CLOUD_TRAIL_MGMT"

pattern AwsLogSourceName_LAMBDA_EXECUTION :: AwsLogSourceName
pattern AwsLogSourceName_LAMBDA_EXECUTION = AwsLogSourceName' "LAMBDA_EXECUTION"

pattern AwsLogSourceName_ROUTE53 :: AwsLogSourceName
pattern AwsLogSourceName_ROUTE53 = AwsLogSourceName' "ROUTE53"

pattern AwsLogSourceName_S3_DATA :: AwsLogSourceName
pattern AwsLogSourceName_S3_DATA = AwsLogSourceName' "S3_DATA"

pattern AwsLogSourceName_SH_FINDINGS :: AwsLogSourceName
pattern AwsLogSourceName_SH_FINDINGS = AwsLogSourceName' "SH_FINDINGS"

pattern AwsLogSourceName_VPC_FLOW :: AwsLogSourceName
pattern AwsLogSourceName_VPC_FLOW = AwsLogSourceName' "VPC_FLOW"

{-# COMPLETE
  AwsLogSourceName_CLOUD_TRAIL_MGMT,
  AwsLogSourceName_LAMBDA_EXECUTION,
  AwsLogSourceName_ROUTE53,
  AwsLogSourceName_S3_DATA,
  AwsLogSourceName_SH_FINDINGS,
  AwsLogSourceName_VPC_FLOW,
  AwsLogSourceName'
  #-}
