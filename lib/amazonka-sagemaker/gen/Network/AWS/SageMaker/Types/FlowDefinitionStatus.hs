{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionStatus
  ( FlowDefinitionStatus
      ( FlowDefinitionStatus',
        FDSInitializing,
        FDSActive,
        FDSFailed,
        FDSDeleting
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FlowDefinitionStatus = FlowDefinitionStatus' Lude.Text
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

pattern FDSInitializing :: FlowDefinitionStatus
pattern FDSInitializing = FlowDefinitionStatus' "Initializing"

pattern FDSActive :: FlowDefinitionStatus
pattern FDSActive = FlowDefinitionStatus' "Active"

pattern FDSFailed :: FlowDefinitionStatus
pattern FDSFailed = FlowDefinitionStatus' "Failed"

pattern FDSDeleting :: FlowDefinitionStatus
pattern FDSDeleting = FlowDefinitionStatus' "Deleting"

{-# COMPLETE
  FDSInitializing,
  FDSActive,
  FDSFailed,
  FDSDeleting,
  FlowDefinitionStatus'
  #-}
