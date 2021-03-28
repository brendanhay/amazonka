{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SSM.Types.ResourceTypeForTagging
  ( ResourceTypeForTagging
    ( ResourceTypeForTagging'
    , ResourceTypeForTaggingDocument
    , ResourceTypeForTaggingManagedInstance
    , ResourceTypeForTaggingMaintenanceWindow
    , ResourceTypeForTaggingParameter
    , ResourceTypeForTaggingPatchBaseline
    , ResourceTypeForTaggingOpsItem
    , fromResourceTypeForTagging
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ResourceTypeForTagging = ResourceTypeForTagging'{fromResourceTypeForTagging
                                                         :: Core.Text}
                                   deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                   Core.Generic)
                                   deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                     Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                     Core.FromJSON, Core.ToXML, Core.FromXML,
                                                     Core.ToText, Core.FromText, Core.ToByteString,
                                                     Core.ToQuery, Core.ToHeader)

pattern ResourceTypeForTaggingDocument :: ResourceTypeForTagging
pattern ResourceTypeForTaggingDocument = ResourceTypeForTagging' "Document"

pattern ResourceTypeForTaggingManagedInstance :: ResourceTypeForTagging
pattern ResourceTypeForTaggingManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern ResourceTypeForTaggingMaintenanceWindow :: ResourceTypeForTagging
pattern ResourceTypeForTaggingMaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern ResourceTypeForTaggingParameter :: ResourceTypeForTagging
pattern ResourceTypeForTaggingParameter = ResourceTypeForTagging' "Parameter"

pattern ResourceTypeForTaggingPatchBaseline :: ResourceTypeForTagging
pattern ResourceTypeForTaggingPatchBaseline = ResourceTypeForTagging' "PatchBaseline"

pattern ResourceTypeForTaggingOpsItem :: ResourceTypeForTagging
pattern ResourceTypeForTaggingOpsItem = ResourceTypeForTagging' "OpsItem"

{-# COMPLETE 
  ResourceTypeForTaggingDocument,

  ResourceTypeForTaggingManagedInstance,

  ResourceTypeForTaggingMaintenanceWindow,

  ResourceTypeForTaggingParameter,

  ResourceTypeForTaggingPatchBaseline,

  ResourceTypeForTaggingOpsItem,
  ResourceTypeForTagging'
  #-}
