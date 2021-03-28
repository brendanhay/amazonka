{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.PermissionModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.PermissionModels
  ( PermissionModels
    ( PermissionModels'
    , PermissionModelsServiceManaged
    , PermissionModelsSelfManaged
    , fromPermissionModels
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype PermissionModels = PermissionModels'{fromPermissionModels
                                             :: Core.Text}
                             deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                             Core.Generic)
                             deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                               Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                               Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                               Core.FromText, Core.ToByteString, Core.ToQuery,
                                               Core.ToHeader)

pattern PermissionModelsServiceManaged :: PermissionModels
pattern PermissionModelsServiceManaged = PermissionModels' "SERVICE_MANAGED"

pattern PermissionModelsSelfManaged :: PermissionModels
pattern PermissionModelsSelfManaged = PermissionModels' "SELF_MANAGED"

{-# COMPLETE 
  PermissionModelsServiceManaged,

  PermissionModelsSelfManaged,
  PermissionModels'
  #-}
