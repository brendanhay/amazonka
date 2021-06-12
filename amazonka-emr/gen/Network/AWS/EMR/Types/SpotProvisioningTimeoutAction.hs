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
-- Module      : Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SpotProvisioningTimeoutAction
  ( SpotProvisioningTimeoutAction
      ( ..,
        SpotProvisioningTimeoutAction_SWITCH_TO_ON_DEMAND,
        SpotProvisioningTimeoutAction_TERMINATE_CLUSTER
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype SpotProvisioningTimeoutAction = SpotProvisioningTimeoutAction'
  { fromSpotProvisioningTimeoutAction ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
