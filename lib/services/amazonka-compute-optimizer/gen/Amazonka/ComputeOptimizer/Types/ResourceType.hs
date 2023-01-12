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
-- Module      : Amazonka.ComputeOptimizer.Types.ResourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ResourceType
  ( ResourceType
      ( ..,
        ResourceType_AutoScalingGroup,
        ResourceType_EbsVolume,
        ResourceType_Ec2Instance,
        ResourceType_EcsService,
        ResourceType_LambdaFunction,
        ResourceType_NotApplicable
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

pattern ResourceType_AutoScalingGroup :: ResourceType
pattern ResourceType_AutoScalingGroup = ResourceType' "AutoScalingGroup"

pattern ResourceType_EbsVolume :: ResourceType
pattern ResourceType_EbsVolume = ResourceType' "EbsVolume"

pattern ResourceType_Ec2Instance :: ResourceType
pattern ResourceType_Ec2Instance = ResourceType' "Ec2Instance"

pattern ResourceType_EcsService :: ResourceType
pattern ResourceType_EcsService = ResourceType' "EcsService"

pattern ResourceType_LambdaFunction :: ResourceType
pattern ResourceType_LambdaFunction = ResourceType' "LambdaFunction"

pattern ResourceType_NotApplicable :: ResourceType
pattern ResourceType_NotApplicable = ResourceType' "NotApplicable"

{-# COMPLETE
  ResourceType_AutoScalingGroup,
  ResourceType_EbsVolume,
  ResourceType_Ec2Instance,
  ResourceType_EcsService,
  ResourceType_LambdaFunction,
  ResourceType_NotApplicable,
  ResourceType'
  #-}
