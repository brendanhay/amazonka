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
-- Module      : Amazonka.RDS.Types.TargetType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.TargetType
  ( TargetType
      ( ..,
        TargetType_RDS_INSTANCE,
        TargetType_RDS_SERVERLESS_ENDPOINT,
        TargetType_TRACKED_CLUSTER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype TargetType = TargetType'
  { fromTargetType ::
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

pattern TargetType_RDS_INSTANCE :: TargetType
pattern TargetType_RDS_INSTANCE = TargetType' "RDS_INSTANCE"

pattern TargetType_RDS_SERVERLESS_ENDPOINT :: TargetType
pattern TargetType_RDS_SERVERLESS_ENDPOINT = TargetType' "RDS_SERVERLESS_ENDPOINT"

pattern TargetType_TRACKED_CLUSTER :: TargetType
pattern TargetType_TRACKED_CLUSTER = TargetType' "TRACKED_CLUSTER"

{-# COMPLETE
  TargetType_RDS_INSTANCE,
  TargetType_RDS_SERVERLESS_ENDPOINT,
  TargetType_TRACKED_CLUSTER,
  TargetType'
  #-}
