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
-- Module      : Amazonka.MigrationHubStrategy.Types.NoPreferenceTargetDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.NoPreferenceTargetDestination
  ( NoPreferenceTargetDestination
      ( ..,
        NoPreferenceTargetDestination_AWS_Elastic_BeanStalk,
        NoPreferenceTargetDestination_AWS_Fargate,
        NoPreferenceTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
        NoPreferenceTargetDestination_Amazon_Elastic_Container_Service__ECS_,
        NoPreferenceTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
        NoPreferenceTargetDestination_None_specified
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype NoPreferenceTargetDestination = NoPreferenceTargetDestination'
  { fromNoPreferenceTargetDestination ::
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

pattern NoPreferenceTargetDestination_AWS_Elastic_BeanStalk :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_AWS_Elastic_BeanStalk = NoPreferenceTargetDestination' "AWS Elastic BeanStalk"

pattern NoPreferenceTargetDestination_AWS_Fargate :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_AWS_Fargate = NoPreferenceTargetDestination' "AWS Fargate"

pattern NoPreferenceTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_ = NoPreferenceTargetDestination' "Amazon Elastic Cloud Compute (EC2)"

pattern NoPreferenceTargetDestination_Amazon_Elastic_Container_Service__ECS_ :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_Amazon_Elastic_Container_Service__ECS_ = NoPreferenceTargetDestination' "Amazon Elastic Container Service (ECS)"

pattern NoPreferenceTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_ = NoPreferenceTargetDestination' "Amazon Elastic Kubernetes Service (EKS)"

pattern NoPreferenceTargetDestination_None_specified :: NoPreferenceTargetDestination
pattern NoPreferenceTargetDestination_None_specified = NoPreferenceTargetDestination' "None specified"

{-# COMPLETE
  NoPreferenceTargetDestination_AWS_Elastic_BeanStalk,
  NoPreferenceTargetDestination_AWS_Fargate,
  NoPreferenceTargetDestination_Amazon_Elastic_Cloud_Compute__EC2_,
  NoPreferenceTargetDestination_Amazon_Elastic_Container_Service__ECS_,
  NoPreferenceTargetDestination_Amazon_Elastic_Kubernetes_Service__EKS_,
  NoPreferenceTargetDestination_None_specified,
  NoPreferenceTargetDestination'
  #-}
