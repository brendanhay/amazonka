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
-- Module      : Amazonka.LicenseManagerLinuxSubscriptions.Types.LinuxSubscriptionsDiscovery
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerLinuxSubscriptions.Types.LinuxSubscriptionsDiscovery
  ( LinuxSubscriptionsDiscovery
      ( ..,
        LinuxSubscriptionsDiscovery_Disabled,
        LinuxSubscriptionsDiscovery_Enabled
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LinuxSubscriptionsDiscovery = LinuxSubscriptionsDiscovery'
  { fromLinuxSubscriptionsDiscovery ::
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

pattern LinuxSubscriptionsDiscovery_Disabled :: LinuxSubscriptionsDiscovery
pattern LinuxSubscriptionsDiscovery_Disabled = LinuxSubscriptionsDiscovery' "Disabled"

pattern LinuxSubscriptionsDiscovery_Enabled :: LinuxSubscriptionsDiscovery
pattern LinuxSubscriptionsDiscovery_Enabled = LinuxSubscriptionsDiscovery' "Enabled"

{-# COMPLETE
  LinuxSubscriptionsDiscovery_Disabled,
  LinuxSubscriptionsDiscovery_Enabled,
  LinuxSubscriptionsDiscovery'
  #-}
