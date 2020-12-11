-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.TrustState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustState
  ( TrustState
      ( TrustState',
        TSCreated,
        TSCreating,
        TSDeleted,
        TSDeleting,
        TSFailed,
        TSUpdateFailed,
        TSUpdated,
        TSUpdating,
        TSVerified,
        TSVerifyFailed,
        TSVerifying
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TrustState = TrustState' Lude.Text
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

pattern TSCreated :: TrustState
pattern TSCreated = TrustState' "Created"

pattern TSCreating :: TrustState
pattern TSCreating = TrustState' "Creating"

pattern TSDeleted :: TrustState
pattern TSDeleted = TrustState' "Deleted"

pattern TSDeleting :: TrustState
pattern TSDeleting = TrustState' "Deleting"

pattern TSFailed :: TrustState
pattern TSFailed = TrustState' "Failed"

pattern TSUpdateFailed :: TrustState
pattern TSUpdateFailed = TrustState' "UpdateFailed"

pattern TSUpdated :: TrustState
pattern TSUpdated = TrustState' "Updated"

pattern TSUpdating :: TrustState
pattern TSUpdating = TrustState' "Updating"

pattern TSVerified :: TrustState
pattern TSVerified = TrustState' "Verified"

pattern TSVerifyFailed :: TrustState
pattern TSVerifyFailed = TrustState' "VerifyFailed"

pattern TSVerifying :: TrustState
pattern TSVerifying = TrustState' "Verifying"

{-# COMPLETE
  TSCreated,
  TSCreating,
  TSDeleted,
  TSDeleting,
  TSFailed,
  TSUpdateFailed,
  TSUpdated,
  TSUpdating,
  TSVerified,
  TSVerifyFailed,
  TSVerifying,
  TrustState'
  #-}
