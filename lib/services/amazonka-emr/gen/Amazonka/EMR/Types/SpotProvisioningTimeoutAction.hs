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
-- Module      : Amazonka.EMR.Types.SpotProvisioningTimeoutAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.SpotProvisioningTimeoutAction
  ( SpotProvisioningTimeoutAction
      ( ..,
        SpotProvisioningTimeoutAction_SWITCH_TO_ON_DEMAND,
        SpotProvisioningTimeoutAction_TERMINATE_CLUSTER
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype SpotProvisioningTimeoutAction = SpotProvisioningTimeoutAction'
  { fromSpotProvisioningTimeoutAction ::
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

pattern SpotProvisioningTimeoutAction_SWITCH_TO_ON_DEMAND :: SpotProvisioningTimeoutAction
pattern SpotProvisioningTimeoutAction_SWITCH_TO_ON_DEMAND = SpotProvisioningTimeoutAction' "SWITCH_TO_ON_DEMAND"

pattern SpotProvisioningTimeoutAction_TERMINATE_CLUSTER :: SpotProvisioningTimeoutAction
pattern SpotProvisioningTimeoutAction_TERMINATE_CLUSTER = SpotProvisioningTimeoutAction' "TERMINATE_CLUSTER"

{-# COMPLETE
  SpotProvisioningTimeoutAction_SWITCH_TO_ON_DEMAND,
  SpotProvisioningTimeoutAction_TERMINATE_CLUSTER,
  SpotProvisioningTimeoutAction'
  #-}
