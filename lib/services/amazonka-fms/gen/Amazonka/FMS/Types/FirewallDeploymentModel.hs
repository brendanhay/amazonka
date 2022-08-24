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
-- Module      : Amazonka.FMS.Types.FirewallDeploymentModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.FirewallDeploymentModel
  ( FirewallDeploymentModel
      ( ..,
        FirewallDeploymentModel_CENTRALIZED,
        FirewallDeploymentModel_DISTRIBUTED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype FirewallDeploymentModel = FirewallDeploymentModel'
  { fromFirewallDeploymentModel ::
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

pattern FirewallDeploymentModel_CENTRALIZED :: FirewallDeploymentModel
pattern FirewallDeploymentModel_CENTRALIZED = FirewallDeploymentModel' "CENTRALIZED"

pattern FirewallDeploymentModel_DISTRIBUTED :: FirewallDeploymentModel
pattern FirewallDeploymentModel_DISTRIBUTED = FirewallDeploymentModel' "DISTRIBUTED"

{-# COMPLETE
  FirewallDeploymentModel_CENTRALIZED,
  FirewallDeploymentModel_DISTRIBUTED,
  FirewallDeploymentModel'
  #-}
