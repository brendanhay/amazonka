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
-- Module      : Amazonka.Account.Types.RegionOptStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Account.Types.RegionOptStatus
  ( RegionOptStatus
      ( ..,
        RegionOptStatus_DISABLED,
        RegionOptStatus_DISABLING,
        RegionOptStatus_ENABLED,
        RegionOptStatus_ENABLED_BY_DEFAULT,
        RegionOptStatus_ENABLING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RegionOptStatus = RegionOptStatus'
  { fromRegionOptStatus ::
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

pattern RegionOptStatus_DISABLED :: RegionOptStatus
pattern RegionOptStatus_DISABLED = RegionOptStatus' "DISABLED"

pattern RegionOptStatus_DISABLING :: RegionOptStatus
pattern RegionOptStatus_DISABLING = RegionOptStatus' "DISABLING"

pattern RegionOptStatus_ENABLED :: RegionOptStatus
pattern RegionOptStatus_ENABLED = RegionOptStatus' "ENABLED"

pattern RegionOptStatus_ENABLED_BY_DEFAULT :: RegionOptStatus
pattern RegionOptStatus_ENABLED_BY_DEFAULT = RegionOptStatus' "ENABLED_BY_DEFAULT"

pattern RegionOptStatus_ENABLING :: RegionOptStatus
pattern RegionOptStatus_ENABLING = RegionOptStatus' "ENABLING"

{-# COMPLETE
  RegionOptStatus_DISABLED,
  RegionOptStatus_DISABLING,
  RegionOptStatus_ENABLED,
  RegionOptStatus_ENABLED_BY_DEFAULT,
  RegionOptStatus_ENABLING,
  RegionOptStatus'
  #-}
