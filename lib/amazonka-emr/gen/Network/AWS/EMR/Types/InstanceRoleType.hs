{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.InstanceRoleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.InstanceRoleType
  ( InstanceRoleType
      ( InstanceRoleType',
        IRTCore,
        IRTMaster,
        IRTTask
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype InstanceRoleType = InstanceRoleType' Lude.Text
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

pattern IRTCore :: InstanceRoleType
pattern IRTCore = InstanceRoleType' "CORE"

pattern IRTMaster :: InstanceRoleType
pattern IRTMaster = InstanceRoleType' "MASTER"

pattern IRTTask :: InstanceRoleType
pattern IRTTask = InstanceRoleType' "TASK"

{-# COMPLETE
  IRTCore,
  IRTMaster,
  IRTTask,
  InstanceRoleType'
  #-}
