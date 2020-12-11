-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Budgets.Types.ActionSubType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Budgets.Types.ActionSubType
  ( ActionSubType
      ( ActionSubType',
        StopEC2Instances,
        StopRDSInstances
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionSubType = ActionSubType' Lude.Text
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

pattern StopEC2Instances :: ActionSubType
pattern StopEC2Instances = ActionSubType' "STOP_EC2_INSTANCES"

pattern StopRDSInstances :: ActionSubType
pattern StopRDSInstances = ActionSubType' "STOP_RDS_INSTANCES"

{-# COMPLETE
  StopEC2Instances,
  StopRDSInstances,
  ActionSubType'
  #-}
