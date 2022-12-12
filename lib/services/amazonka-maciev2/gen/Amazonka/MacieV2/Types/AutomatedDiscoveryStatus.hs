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
-- Module      : Amazonka.MacieV2.Types.AutomatedDiscoveryStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AutomatedDiscoveryStatus
  ( AutomatedDiscoveryStatus
      ( ..,
        AutomatedDiscoveryStatus_DISABLED,
        AutomatedDiscoveryStatus_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of the automated sensitive data discovery configuration for
-- an Amazon Macie account. Valid values are:
newtype AutomatedDiscoveryStatus = AutomatedDiscoveryStatus'
  { fromAutomatedDiscoveryStatus ::
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

pattern AutomatedDiscoveryStatus_DISABLED :: AutomatedDiscoveryStatus
pattern AutomatedDiscoveryStatus_DISABLED = AutomatedDiscoveryStatus' "DISABLED"

pattern AutomatedDiscoveryStatus_ENABLED :: AutomatedDiscoveryStatus
pattern AutomatedDiscoveryStatus_ENABLED = AutomatedDiscoveryStatus' "ENABLED"

{-# COMPLETE
  AutomatedDiscoveryStatus_DISABLED,
  AutomatedDiscoveryStatus_ENABLED,
  AutomatedDiscoveryStatus'
  #-}
