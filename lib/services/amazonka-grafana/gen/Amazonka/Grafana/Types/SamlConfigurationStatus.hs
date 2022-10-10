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
-- Module      : Amazonka.Grafana.Types.SamlConfigurationStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.SamlConfigurationStatus
  ( SamlConfigurationStatus
      ( ..,
        SamlConfigurationStatus_CONFIGURED,
        SamlConfigurationStatus_NOT_CONFIGURED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SamlConfigurationStatus = SamlConfigurationStatus'
  { fromSamlConfigurationStatus ::
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

pattern SamlConfigurationStatus_CONFIGURED :: SamlConfigurationStatus
pattern SamlConfigurationStatus_CONFIGURED = SamlConfigurationStatus' "CONFIGURED"

pattern SamlConfigurationStatus_NOT_CONFIGURED :: SamlConfigurationStatus
pattern SamlConfigurationStatus_NOT_CONFIGURED = SamlConfigurationStatus' "NOT_CONFIGURED"

{-# COMPLETE
  SamlConfigurationStatus_CONFIGURED,
  SamlConfigurationStatus_NOT_CONFIGURED,
  SamlConfigurationStatus'
  #-}
