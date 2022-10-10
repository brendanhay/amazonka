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
        UsageType_ECR_RESCAN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UsageType = UsageType'
  { fromUsageType ::
      Core.Text
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

pattern UsageType_EC2_INSTANCE_HOURS :: UsageType
pattern UsageType_EC2_INSTANCE_HOURS = UsageType' "EC2_INSTANCE_HOURS"

pattern UsageType_ECR_INITIAL_SCAN :: UsageType
pattern UsageType_ECR_INITIAL_SCAN = UsageType' "ECR_INITIAL_SCAN"

pattern UsageType_ECR_RESCAN :: UsageType
pattern UsageType_ECR_RESCAN = UsageType' "ECR_RESCAN"

{-# COMPLETE
  UsageType_EC2_INSTANCE_HOURS,
  UsageType_ECR_INITIAL_SCAN,
  UsageType_ECR_RESCAN,
  UsageType'
  #-}
