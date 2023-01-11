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
-- Module      : Amazonka.MigrationHubStrategy.Types.SelfManageTargetDestination
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.SelfManageTargetDestination
  ( SelfManageTargetDestination
      ( ..,
        SelfManageTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
        SelfManageTargetDestination_Amazon_Elastic_Container_Service__ECS_,
        SelfManageTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
        SelfManageTargetDestination_None_specified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SelfManageTargetDestination = SelfManageTargetDestination'
  { fromSelfManageTargetDestination ::
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

pattern SelfManageTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ :: SelfManageTargetDestination
pattern SelfManageTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ = SelfManageTargetDestination' "Amazon Elastic Cloud Compute (EC2)"

pattern SelfManageTargetDestination_Amazon_Elastic_Container_Service__ECS_ :: SelfManageTargetDestination
pattern SelfManageTargetDestination_Amazon_Elastic_Container_Service__ECS_ = SelfManageTargetDestination' "Amazon Elastic Container Service (ECS)"

pattern SelfManageTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ :: SelfManageTargetDestination
pattern SelfManageTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ = SelfManageTargetDestination' "Amazon Elastic Kubernetes Service (EKS)"

pattern SelfManageTargetDestination_None_specified :: SelfManageTargetDestination
pattern SelfManageTargetDestination_None_specified = SelfManageTargetDestination' "None specified"

{-# COMPLETE
  SelfManageTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
  SelfManageTargetDestination_Amazon_Elastic_Container_Service__ECS_,
  SelfManageTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
  SelfManageTargetDestination_None_specified,
  SelfManageTargetDestination'
  #-}
