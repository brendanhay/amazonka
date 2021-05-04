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
-- Module      : Network.AWS.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResourceTypeForTagging
  ( ResourceTypeForTagging
      ( ..,
        ResourceTypeForTagging_Document,
        ResourceTypeForTagging_MaintenanceWindow,
        ResourceTypeForTagging_ManagedInstance,
        ResourceTypeForTagging_OpsItem,
        ResourceTypeForTagging_Parameter,
        ResourceTypeForTagging_PatchBaseline
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ResourceTypeForTagging = ResourceTypeForTagging'
  { fromResourceTypeForTagging ::
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

pattern ResourceTypeForTagging_Document :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Document = ResourceTypeForTagging' "Document"

pattern ResourceTypeForTagging_MaintenanceWindow :: ResourceTypeForTagging
pattern ResourceTypeForTagging_MaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern ResourceTypeForTagging_ManagedInstance :: ResourceTypeForTagging
pattern ResourceTypeForTagging_ManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern ResourceTypeForTagging_OpsItem :: ResourceTypeForTagging
pattern ResourceTypeForTagging_OpsItem = ResourceTypeForTagging' "OpsItem"

pattern ResourceTypeForTagging_Parameter :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Parameter = ResourceTypeForTagging' "Parameter"

pattern ResourceTypeForTagging_PatchBaseline :: ResourceTypeForTagging
pattern ResourceTypeForTagging_PatchBaseline = ResourceTypeForTagging' "PatchBaseline"

{-# COMPLETE
  ResourceTypeForTagging_Document,
  ResourceTypeForTagging_MaintenanceWindow,
  ResourceTypeForTagging_ManagedInstance,
  ResourceTypeForTagging_OpsItem,
  ResourceTypeForTagging_Parameter,
  ResourceTypeForTagging_PatchBaseline,
  ResourceTypeForTagging'
  #-}
