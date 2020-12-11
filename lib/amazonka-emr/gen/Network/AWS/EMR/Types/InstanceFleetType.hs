-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceFleetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceFleetType
  ( InstanceFleetType
      ( InstanceFleetType',
        IFTCore,
        IFTMaster,
        IFTTask
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceFleetType = InstanceFleetType' Lude.Text
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

pattern IFTCore :: InstanceFleetType
pattern IFTCore = InstanceFleetType' "CORE"

pattern IFTMaster :: InstanceFleetType
pattern IFTMaster = InstanceFleetType' "MASTER"

pattern IFTTask :: InstanceFleetType
pattern IFTTask = InstanceFleetType' "TASK"

{-# COMPLETE
  IFTCore,
  IFTMaster,
  IFTTask,
  InstanceFleetType'
  #-}
