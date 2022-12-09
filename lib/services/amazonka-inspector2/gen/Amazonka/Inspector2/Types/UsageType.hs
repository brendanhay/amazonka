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
-- Module      : Amazonka.Inspector2.Types.UsageType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.UsageType
  ( UsageType
      ( ..,
        UsageType_EC2_INSTANCE_HOURS,
        UsageType_ECR_INITIAL_SCAN,
        UsageType_ECR_RESCAN,
        UsageType_LAMBDA_FUNCTION_HOURS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype UsageType = UsageType'
  { fromUsageType ::
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

pattern UsageType_EC2_INSTANCE_HOURS :: UsageType
pattern UsageType_EC2_INSTANCE_HOURS = UsageType' "EC2_INSTANCE_HOURS"

pattern UsageType_ECR_INITIAL_SCAN :: UsageType
pattern UsageType_ECR_INITIAL_SCAN = UsageType' "ECR_INITIAL_SCAN"

pattern UsageType_ECR_RESCAN :: UsageType
pattern UsageType_ECR_RESCAN = UsageType' "ECR_RESCAN"

pattern UsageType_LAMBDA_FUNCTION_HOURS :: UsageType
pattern UsageType_LAMBDA_FUNCTION_HOURS = UsageType' "LAMBDA_FUNCTION_HOURS"

{-# COMPLETE
  UsageType_EC2_INSTANCE_HOURS,
  UsageType_ECR_INITIAL_SCAN,
  UsageType_ECR_RESCAN,
  UsageType_LAMBDA_FUNCTION_HOURS,
  UsageType'
  #-}
