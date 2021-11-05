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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentOption
  ( DeploymentOption
      ( ..,
        DeploymentOption_WITHOUT_TRAFFIC_CONTROL,
        DeploymentOption_WITH_TRAFFIC_CONTROL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeploymentOption = DeploymentOption'
  { fromDeploymentOption ::
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

pattern DeploymentOption_WITHOUT_TRAFFIC_CONTROL :: DeploymentOption
pattern DeploymentOption_WITHOUT_TRAFFIC_CONTROL = DeploymentOption' "WITHOUT_TRAFFIC_CONTROL"

pattern DeploymentOption_WITH_TRAFFIC_CONTROL :: DeploymentOption
pattern DeploymentOption_WITH_TRAFFIC_CONTROL = DeploymentOption' "WITH_TRAFFIC_CONTROL"

{-# COMPLETE
  DeploymentOption_WITHOUT_TRAFFIC_CONTROL,
  DeploymentOption_WITH_TRAFFIC_CONTROL,
  DeploymentOption'
  #-}
