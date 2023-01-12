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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TargetType = TargetType'
  { fromTargetType ::
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
