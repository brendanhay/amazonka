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
        NASFailed,
        NASInProgress,
        NASSuccess
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

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
newtype NodeAssociationStatus = NodeAssociationStatus' Lude.Text
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

pattern NASFailed :: NodeAssociationStatus
pattern NASFailed = NodeAssociationStatus' "FAILED"

pattern NASInProgress :: NodeAssociationStatus
pattern NASInProgress = NodeAssociationStatus' "IN_PROGRESS"

pattern NASSuccess :: NodeAssociationStatus
pattern NASSuccess = NodeAssociationStatus' "SUCCESS"

{-# COMPLETE
  NASFailed,
  NASInProgress,
  NASSuccess,
  NodeAssociationStatus'
  #-}
