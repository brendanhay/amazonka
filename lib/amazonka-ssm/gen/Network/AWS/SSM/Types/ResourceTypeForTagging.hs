-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceTypeForTagging
  ( ResourceTypeForTagging
      ( ResourceTypeForTagging',
        RTFTDocument,
        RTFTMaintenanceWindow,
        RTFTManagedInstance,
        RTFTOpsItem,
        RTFTParameter,
        RTFTPatchBaseline
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ResourceTypeForTagging = ResourceTypeForTagging' Lude.Text
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

pattern RTFTDocument :: ResourceTypeForTagging
pattern RTFTDocument = ResourceTypeForTagging' "Document"

pattern RTFTMaintenanceWindow :: ResourceTypeForTagging
pattern RTFTMaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern RTFTManagedInstance :: ResourceTypeForTagging
pattern RTFTManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern RTFTOpsItem :: ResourceTypeForTagging
pattern RTFTOpsItem = ResourceTypeForTagging' "OpsItem"

pattern RTFTParameter :: ResourceTypeForTagging
pattern RTFTParameter = ResourceTypeForTagging' "Parameter"

pattern RTFTPatchBaseline :: ResourceTypeForTagging
pattern RTFTPatchBaseline = ResourceTypeForTagging' "PatchBaseline"

{-# COMPLETE
  RTFTDocument,
  RTFTMaintenanceWindow,
  RTFTManagedInstance,
  RTFTOpsItem,
  RTFTParameter,
  RTFTPatchBaseline,
  ResourceTypeForTagging'
  #-}
