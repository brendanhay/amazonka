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
-- Module      : Amazonka.CodeDeploy.Types.MinimumHealthyHostsType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.MinimumHealthyHostsType
  ( MinimumHealthyHostsType
      ( ..,
        MinimumHealthyHostsType_FLEET_PERCENT,
        MinimumHealthyHostsType_HOST_COUNT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype MinimumHealthyHostsType = MinimumHealthyHostsType'
  { fromMinimumHealthyHostsType ::
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

pattern MinimumHealthyHostsType_FLEET_PERCENT :: MinimumHealthyHostsType
pattern MinimumHealthyHostsType_FLEET_PERCENT = MinimumHealthyHostsType' "FLEET_PERCENT"

pattern MinimumHealthyHostsType_HOST_COUNT :: MinimumHealthyHostsType
pattern MinimumHealthyHostsType_HOST_COUNT = MinimumHealthyHostsType' "HOST_COUNT"

{-# COMPLETE
  MinimumHealthyHostsType_FLEET_PERCENT,
  MinimumHealthyHostsType_HOST_COUNT,
  MinimumHealthyHostsType'
  #-}
