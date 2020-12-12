{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ServiceNamespace
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ServiceNamespace
  ( ServiceNamespace
      ( ServiceNamespace',
        Autoscaling,
        Dynamodb,
        EC2,
        Ecs,
        RDS
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ServiceNamespace = ServiceNamespace' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Autoscaling :: ServiceNamespace
pattern Autoscaling = ServiceNamespace' "autoscaling"

pattern Dynamodb :: ServiceNamespace
pattern Dynamodb = ServiceNamespace' "dynamodb"

pattern EC2 :: ServiceNamespace
pattern EC2 = ServiceNamespace' "ec2"

pattern Ecs :: ServiceNamespace
pattern Ecs = ServiceNamespace' "ecs"

pattern RDS :: ServiceNamespace
pattern RDS = ServiceNamespace' "rds"

{-# COMPLETE
  Autoscaling,
  Dynamodb,
  EC2,
  Ecs,
  RDS,
  ServiceNamespace'
  #-}
