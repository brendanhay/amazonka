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
-- Module      : Network.AWS.DirectoryService.Types.ShareStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_Deleted,
        ShareStatus_Deleting,
        ShareStatus_PendingAcceptance,
        ShareStatus_RejectFailed,
        ShareStatus_Rejected,
        ShareStatus_Rejecting,
        ShareStatus_ShareFailed,
        ShareStatus_Shared,
        ShareStatus_Sharing
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
      Core.Text
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

pattern ShareStatus_Deleted :: ShareStatus
pattern ShareStatus_Deleted = ShareStatus' "Deleted"

pattern ShareStatus_Deleting :: ShareStatus
pattern ShareStatus_Deleting = ShareStatus' "Deleting"

pattern ShareStatus_PendingAcceptance :: ShareStatus
pattern ShareStatus_PendingAcceptance = ShareStatus' "PendingAcceptance"

pattern ShareStatus_RejectFailed :: ShareStatus
pattern ShareStatus_RejectFailed = ShareStatus' "RejectFailed"

pattern ShareStatus_Rejected :: ShareStatus
pattern ShareStatus_Rejected = ShareStatus' "Rejected"

pattern ShareStatus_Rejecting :: ShareStatus
pattern ShareStatus_Rejecting = ShareStatus' "Rejecting"

pattern ShareStatus_ShareFailed :: ShareStatus
pattern ShareStatus_ShareFailed = ShareStatus' "ShareFailed"

pattern ShareStatus_Shared :: ShareStatus
pattern ShareStatus_Shared = ShareStatus' "Shared"

pattern ShareStatus_Sharing :: ShareStatus
pattern ShareStatus_Sharing = ShareStatus' "Sharing"

{-# COMPLETE
  ShareStatus_Deleted,
  ShareStatus_Deleting,
  ShareStatus_PendingAcceptance,
  ShareStatus_RejectFailed,
  ShareStatus_Rejected,
  ShareStatus_Rejecting,
  ShareStatus_ShareFailed,
  ShareStatus_Shared,
  ShareStatus_Sharing,
  ShareStatus'
  #-}
