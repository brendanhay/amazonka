{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
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

import qualified Network.AWS.Core as Core

newtype ResourceTypeForTagging = ResourceTypeForTagging'
  { fromResourceTypeForTagging ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
