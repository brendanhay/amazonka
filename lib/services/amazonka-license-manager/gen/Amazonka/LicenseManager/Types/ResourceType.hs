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
-- Module      : Amazonka.LicenseManager.Types.ResourceType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_EC2_AMI,
        ResourceType_EC2_HOST,
        ResourceType_EC2_INSTANCE,
        ResourceType_RDS,
        ResourceType_SYSTEMS_MANAGER_MANAGED_INSTANCE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceType = ResourceType'
  { fromResourceType ::
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

pattern ResourceType_EC2_AMI :: ResourceType
pattern ResourceType_EC2_AMI = ResourceType' "EC2_AMI"

pattern ResourceType_EC2_HOST :: ResourceType
pattern ResourceType_EC2_HOST = ResourceType' "EC2_HOST"

pattern ResourceType_EC2_INSTANCE :: ResourceType
pattern ResourceType_EC2_INSTANCE = ResourceType' "EC2_INSTANCE"

pattern ResourceType_RDS :: ResourceType
pattern ResourceType_RDS = ResourceType' "RDS"

pattern ResourceType_SYSTEMS_MANAGER_MANAGED_INSTANCE :: ResourceType
pattern ResourceType_SYSTEMS_MANAGER_MANAGED_INSTANCE = ResourceType' "SYSTEMS_MANAGER_MANAGED_INSTANCE"

{-# COMPLETE
  ResourceType_EC2_AMI,
  ResourceType_EC2_HOST,
  ResourceType_EC2_INSTANCE,
  ResourceType_RDS,
  ResourceType_SYSTEMS_MANAGER_MANAGED_INSTANCE,
  ResourceType'
  #-}
