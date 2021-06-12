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
-- Module      : Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProvisioningArtifactType
  ( ProvisioningArtifactType
      ( ..,
        ProvisioningArtifactType_CLOUD_FORMATION_TEMPLATE,
        ProvisioningArtifactType_MARKETPLACE_AMI,
        ProvisioningArtifactType_MARKETPLACE_CAR
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProvisioningArtifactType = ProvisioningArtifactType'
  { fromProvisioningArtifactType ::
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

pattern ProvisioningArtifactType_CLOUD_FORMATION_TEMPLATE :: ProvisioningArtifactType
pattern ProvisioningArtifactType_CLOUD_FORMATION_TEMPLATE = ProvisioningArtifactType' "CLOUD_FORMATION_TEMPLATE"

pattern ProvisioningArtifactType_MARKETPLACE_AMI :: ProvisioningArtifactType
pattern ProvisioningArtifactType_MARKETPLACE_AMI = ProvisioningArtifactType' "MARKETPLACE_AMI"

pattern ProvisioningArtifactType_MARKETPLACE_CAR :: ProvisioningArtifactType
pattern ProvisioningArtifactType_MARKETPLACE_CAR = ProvisioningArtifactType' "MARKETPLACE_CAR"

{-# COMPLETE
  ProvisioningArtifactType_CLOUD_FORMATION_TEMPLATE,
  ProvisioningArtifactType_MARKETPLACE_AMI,
  ProvisioningArtifactType_MARKETPLACE_CAR,
  ProvisioningArtifactType'
  #-}
