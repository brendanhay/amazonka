{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorksCM.Types.NodeAssociationStatus
  ( NodeAssociationStatus
      ( NodeAssociationStatus',
        NodeAssociationStatusSuccess,
        NodeAssociationStatusFailed,
        NodeAssociationStatusInProgress,
        fromNodeAssociationStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The status of the association or disassociation request.
--
-- __Possible values:__
--
--     * @SUCCESS@ : The association or disassociation succeeded.
--
--
--     * @FAILED@ : The association or disassociation failed.
--
--
--     * @IN_PROGRESS@ : The association or disassociation is still in progress.
newtype NodeAssociationStatus = NodeAssociationStatus'
  { fromNodeAssociationStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern NodeAssociationStatusSuccess :: NodeAssociationStatus
pattern NodeAssociationStatusSuccess = NodeAssociationStatus' "SUCCESS"

pattern NodeAssociationStatusFailed :: NodeAssociationStatus
pattern NodeAssociationStatusFailed = NodeAssociationStatus' "FAILED"

pattern NodeAssociationStatusInProgress :: NodeAssociationStatus
pattern NodeAssociationStatusInProgress = NodeAssociationStatus' "IN_PROGRESS"

{-# COMPLETE
  NodeAssociationStatusSuccess,
  NodeAssociationStatusFailed,
  NodeAssociationStatusInProgress,
  NodeAssociationStatus'
  #-}
