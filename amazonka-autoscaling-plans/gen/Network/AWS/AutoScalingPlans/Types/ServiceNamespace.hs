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
-- Module      : Network.AWS.AutoScalingPlans.Types.ServiceNamespace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ServiceNamespace
  ( ServiceNamespace
      ( ..,
        ServiceNamespace_Autoscaling,
        ServiceNamespace_Dynamodb,
        ServiceNamespace_Ec2,
        ServiceNamespace_Ecs,
        ServiceNamespace_Rds
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ServiceNamespace = ServiceNamespace'
  { fromServiceNamespace ::
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

pattern ServiceNamespace_Autoscaling :: ServiceNamespace
pattern ServiceNamespace_Autoscaling = ServiceNamespace' "autoscaling"

pattern ServiceNamespace_Dynamodb :: ServiceNamespace
pattern ServiceNamespace_Dynamodb = ServiceNamespace' "dynamodb"

pattern ServiceNamespace_Ec2 :: ServiceNamespace
pattern ServiceNamespace_Ec2 = ServiceNamespace' "ec2"

pattern ServiceNamespace_Ecs :: ServiceNamespace
pattern ServiceNamespace_Ecs = ServiceNamespace' "ecs"

pattern ServiceNamespace_Rds :: ServiceNamespace
pattern ServiceNamespace_Rds = ServiceNamespace' "rds"

{-# COMPLETE
  ServiceNamespace_Autoscaling,
  ServiceNamespace_Dynamodb,
  ServiceNamespace_Ec2,
  ServiceNamespace_Ecs,
  ServiceNamespace_Rds,
  ServiceNamespace'
  #-}
