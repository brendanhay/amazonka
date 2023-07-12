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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningArtifactType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningArtifactType
  ( ProvisioningArtifactType
      ( ..,
        ProvisioningArtifactType_CLOUD_FORMATION_TEMPLATE,
        ProvisioningArtifactType_MARKETPLACE_AMI,
        ProvisioningArtifactType_MARKETPLACE_CAR
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ProvisioningArtifactType = ProvisioningArtifactType'
  { fromProvisioningArtifactType ::
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
