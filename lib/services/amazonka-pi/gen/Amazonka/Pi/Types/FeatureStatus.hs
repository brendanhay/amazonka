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
-- Module      : Amazonka.Pi.Types.FeatureStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pi.Types.FeatureStatus
  ( FeatureStatus
      ( ..,
        FeatureStatus_DISABLED,
        FeatureStatus_DISABLED_PENDING_REBOOT,
        FeatureStatus_ENABLED,
        FeatureStatus_ENABLED_PENDING_REBOOT,
        FeatureStatus_UNKNOWN,
        FeatureStatus_UNSUPPORTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FeatureStatus = FeatureStatus'
  { fromFeatureStatus ::
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

pattern FeatureStatus_DISABLED :: FeatureStatus
pattern FeatureStatus_DISABLED = FeatureStatus' "DISABLED"

pattern FeatureStatus_DISABLED_PENDING_REBOOT :: FeatureStatus
pattern FeatureStatus_DISABLED_PENDING_REBOOT = FeatureStatus' "DISABLED_PENDING_REBOOT"

pattern FeatureStatus_ENABLED :: FeatureStatus
pattern FeatureStatus_ENABLED = FeatureStatus' "ENABLED"

pattern FeatureStatus_ENABLED_PENDING_REBOOT :: FeatureStatus
pattern FeatureStatus_ENABLED_PENDING_REBOOT = FeatureStatus' "ENABLED_PENDING_REBOOT"

pattern FeatureStatus_UNKNOWN :: FeatureStatus
pattern FeatureStatus_UNKNOWN = FeatureStatus' "UNKNOWN"

pattern FeatureStatus_UNSUPPORTED :: FeatureStatus
pattern FeatureStatus_UNSUPPORTED = FeatureStatus' "UNSUPPORTED"

{-# COMPLETE
  FeatureStatus_DISABLED,
  FeatureStatus_DISABLED_PENDING_REBOOT,
  FeatureStatus_ENABLED,
  FeatureStatus_ENABLED_PENDING_REBOOT,
  FeatureStatus_UNKNOWN,
  FeatureStatus_UNSUPPORTED,
  FeatureStatus'
  #-}
