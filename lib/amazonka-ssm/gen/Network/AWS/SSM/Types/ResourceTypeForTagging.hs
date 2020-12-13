{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Document,
        ManagedInstance,
        MaintenanceWindow,
        Parameter,
        PatchBaseline,
        OpsItem
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

pattern Document :: ResourceTypeForTagging
pattern Document = ResourceTypeForTagging' "Document"

pattern ManagedInstance :: ResourceTypeForTagging
pattern ManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern MaintenanceWindow :: ResourceTypeForTagging
pattern MaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern Parameter :: ResourceTypeForTagging
pattern Parameter = ResourceTypeForTagging' "Parameter"

pattern PatchBaseline :: ResourceTypeForTagging
pattern PatchBaseline = ResourceTypeForTagging' "PatchBaseline"

pattern OpsItem :: ResourceTypeForTagging
pattern OpsItem = ResourceTypeForTagging' "OpsItem"

{-# COMPLETE
  Document,
  ManagedInstance,
  MaintenanceWindow,
  Parameter,
  PatchBaseline,
  OpsItem,
  ResourceTypeForTagging'
  #-}
