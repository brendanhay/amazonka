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
-- Module      : Amazonka.SSM.Types.ResourceTypeForTagging
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ResourceTypeForTagging
  ( ResourceTypeForTagging
      ( ..,
        ResourceTypeForTagging_Association,
        ResourceTypeForTagging_Automation,
        ResourceTypeForTagging_Document,
        ResourceTypeForTagging_MaintenanceWindow,
        ResourceTypeForTagging_ManagedInstance,
        ResourceTypeForTagging_OpsItem,
        ResourceTypeForTagging_OpsMetadata,
        ResourceTypeForTagging_Parameter,
        ResourceTypeForTagging_PatchBaseline
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceTypeForTagging = ResourceTypeForTagging'
  { fromResourceTypeForTagging ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern ResourceTypeForTagging_Association :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Association = ResourceTypeForTagging' "Association"

pattern ResourceTypeForTagging_Automation :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Automation = ResourceTypeForTagging' "Automation"

pattern ResourceTypeForTagging_Document :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Document = ResourceTypeForTagging' "Document"

pattern ResourceTypeForTagging_MaintenanceWindow :: ResourceTypeForTagging
pattern ResourceTypeForTagging_MaintenanceWindow = ResourceTypeForTagging' "MaintenanceWindow"

pattern ResourceTypeForTagging_ManagedInstance :: ResourceTypeForTagging
pattern ResourceTypeForTagging_ManagedInstance = ResourceTypeForTagging' "ManagedInstance"

pattern ResourceTypeForTagging_OpsItem :: ResourceTypeForTagging
pattern ResourceTypeForTagging_OpsItem = ResourceTypeForTagging' "OpsItem"

pattern ResourceTypeForTagging_OpsMetadata :: ResourceTypeForTagging
pattern ResourceTypeForTagging_OpsMetadata = ResourceTypeForTagging' "OpsMetadata"

pattern ResourceTypeForTagging_Parameter :: ResourceTypeForTagging
pattern ResourceTypeForTagging_Parameter = ResourceTypeForTagging' "Parameter"

pattern ResourceTypeForTagging_PatchBaseline :: ResourceTypeForTagging
pattern ResourceTypeForTagging_PatchBaseline = ResourceTypeForTagging' "PatchBaseline"

{-# COMPLETE
  ResourceTypeForTagging_Association,
  ResourceTypeForTagging_Automation,
  ResourceTypeForTagging_Document,
  ResourceTypeForTagging_MaintenanceWindow,
  ResourceTypeForTagging_ManagedInstance,
  ResourceTypeForTagging_OpsItem,
  ResourceTypeForTagging_OpsMetadata,
  ResourceTypeForTagging_Parameter,
  ResourceTypeForTagging_PatchBaseline,
  ResourceTypeForTagging'
  #-}
