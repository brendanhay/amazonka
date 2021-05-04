{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.GreenFleetProvisioningAction
  ( GreenFleetProvisioningAction
      ( ..,
        GreenFleetProvisioningAction_COPY_AUTO_SCALING_GROUP,
        GreenFleetProvisioningAction_DISCOVER_EXISTING
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype GreenFleetProvisioningAction = GreenFleetProvisioningAction'
  { fromGreenFleetProvisioningAction ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
