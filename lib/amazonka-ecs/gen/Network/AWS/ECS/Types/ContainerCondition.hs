{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ContainerCondition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ContainerCondition
  ( ContainerCondition
      ( ContainerCondition',
        Start,
        Complete,
        Success,
        Healthy
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContainerCondition = ContainerCondition' Lude.Text
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

pattern Start :: ContainerCondition
pattern Start = ContainerCondition' "START"

pattern Complete :: ContainerCondition
pattern Complete = ContainerCondition' "COMPLETE"

pattern Success :: ContainerCondition
pattern Success = ContainerCondition' "SUCCESS"

pattern Healthy :: ContainerCondition
pattern Healthy = ContainerCondition' "HEALTHY"

{-# COMPLETE
  Start,
  Complete,
  Success,
  Healthy,
  ContainerCondition'
  #-}
