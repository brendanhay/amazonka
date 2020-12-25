{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        TrustStateCreating,
        TrustStateCreated,
        TrustStateVerifying,
        TrustStateVerifyFailed,
        TrustStateVerified,
        TrustStateUpdating,
        TrustStateUpdateFailed,
        TrustStateUpdated,
        TrustStateDeleting,
        TrustStateDeleted,
        TrustStateFailed,
        fromTrustState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TrustState = TrustState' {fromTrustState :: Core.Text}
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

pattern TrustStateCreating :: TrustState
pattern TrustStateCreating = TrustState' "Creating"

pattern TrustStateCreated :: TrustState
pattern TrustStateCreated = TrustState' "Created"

pattern TrustStateVerifying :: TrustState
pattern TrustStateVerifying = TrustState' "Verifying"

pattern TrustStateVerifyFailed :: TrustState
pattern TrustStateVerifyFailed = TrustState' "VerifyFailed"

pattern TrustStateVerified :: TrustState
pattern TrustStateVerified = TrustState' "Verified"

pattern TrustStateUpdating :: TrustState
pattern TrustStateUpdating = TrustState' "Updating"

pattern TrustStateUpdateFailed :: TrustState
pattern TrustStateUpdateFailed = TrustState' "UpdateFailed"

pattern TrustStateUpdated :: TrustState
pattern TrustStateUpdated = TrustState' "Updated"

pattern TrustStateDeleting :: TrustState
pattern TrustStateDeleting = TrustState' "Deleting"

pattern TrustStateDeleted :: TrustState
pattern TrustStateDeleted = TrustState' "Deleted"

pattern TrustStateFailed :: TrustState
pattern TrustStateFailed = TrustState' "Failed"

{-# COMPLETE
  TrustStateCreating,
  TrustStateCreated,
  TrustStateVerifying,
  TrustStateVerifyFailed,
  TrustStateVerified,
  TrustStateUpdating,
  TrustStateUpdateFailed,
  TrustStateUpdated,
  TrustStateDeleting,
  TrustStateDeleted,
  TrustStateFailed,
  TrustState'
  #-}
