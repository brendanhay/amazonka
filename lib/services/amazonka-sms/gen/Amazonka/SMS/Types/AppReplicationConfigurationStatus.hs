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
-- Module      : Amazonka.SMS.Types.AppReplicationConfigurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SMS.Types.AppReplicationConfigurationStatus
  ( AppReplicationConfigurationStatus
      ( ..,
        AppReplicationConfigurationStatus_CONFIGURED,
        AppReplicationConfigurationStatus_NOT_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AppReplicationConfigurationStatus = AppReplicationConfigurationStatus'
  { fromAppReplicationConfigurationStatus ::
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

pattern AppReplicationConfigurationStatus_CONFIGURED :: AppReplicationConfigurationStatus
pattern AppReplicationConfigurationStatus_CONFIGURED = AppReplicationConfigurationStatus' "CONFIGURED"

pattern AppReplicationConfigurationStatus_NOT_CONFIGURED :: AppReplicationConfigurationStatus
pattern AppReplicationConfigurationStatus_NOT_CONFIGURED = AppReplicationConfigurationStatus' "NOT_CONFIGURED"

{-# COMPLETE
  AppReplicationConfigurationStatus_CONFIGURED,
  AppReplicationConfigurationStatus_NOT_CONFIGURED,
  AppReplicationConfigurationStatus'
  #-}
