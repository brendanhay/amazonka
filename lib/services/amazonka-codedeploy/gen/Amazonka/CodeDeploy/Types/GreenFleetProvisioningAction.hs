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
-- Module      : Amazonka.CodeDeploy.Types.GreenFleetProvisioningAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.GreenFleetProvisioningAction
  ( GreenFleetProvisioningAction
      ( ..,
        GreenFleetProvisioningAction_COPY_AUTO_SCALING_GROUP,
        GreenFleetProvisioningAction_DISCOVER_EXISTING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype GreenFleetProvisioningAction = GreenFleetProvisioningAction'
  { fromGreenFleetProvisioningAction ::
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

pattern GreenFleetProvisioningAction_COPY_AUTO_SCALING_GROUP :: GreenFleetProvisioningAction
pattern GreenFleetProvisioningAction_COPY_AUTO_SCALING_GROUP = GreenFleetProvisioningAction' "COPY_AUTO_SCALING_GROUP"

pattern GreenFleetProvisioningAction_DISCOVER_EXISTING :: GreenFleetProvisioningAction
pattern GreenFleetProvisioningAction_DISCOVER_EXISTING = GreenFleetProvisioningAction' "DISCOVER_EXISTING"

{-# COMPLETE
  GreenFleetProvisioningAction_COPY_AUTO_SCALING_GROUP,
  GreenFleetProvisioningAction_DISCOVER_EXISTING,
  GreenFleetProvisioningAction'
  #-}
