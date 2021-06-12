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
-- Module      : Network.AWS.DirectoryService.Types.TrustState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.TrustState
  ( TrustState
      ( ..,
        TrustState_Created,
        TrustState_Creating,
        TrustState_Deleted,
        TrustState_Deleting,
        TrustState_Failed,
        TrustState_UpdateFailed,
        TrustState_Updated,
        TrustState_Updating,
        TrustState_Verified,
        TrustState_VerifyFailed,
        TrustState_Verifying
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype TrustState = TrustState'
  { fromTrustState ::
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

pattern TrustState_Created :: TrustState
pattern TrustState_Created = TrustState' "Created"

pattern TrustState_Creating :: TrustState
pattern TrustState_Creating = TrustState' "Creating"

pattern TrustState_Deleted :: TrustState
pattern TrustState_Deleted = TrustState' "Deleted"

pattern TrustState_Deleting :: TrustState
pattern TrustState_Deleting = TrustState' "Deleting"

pattern TrustState_Failed :: TrustState
pattern TrustState_Failed = TrustState' "Failed"

pattern TrustState_UpdateFailed :: TrustState
pattern TrustState_UpdateFailed = TrustState' "UpdateFailed"

pattern TrustState_Updated :: TrustState
pattern TrustState_Updated = TrustState' "Updated"

pattern TrustState_Updating :: TrustState
pattern TrustState_Updating = TrustState' "Updating"

pattern TrustState_Verified :: TrustState
pattern TrustState_Verified = TrustState' "Verified"

pattern TrustState_VerifyFailed :: TrustState
pattern TrustState_VerifyFailed = TrustState' "VerifyFailed"

pattern TrustState_Verifying :: TrustState
pattern TrustState_Verifying = TrustState' "Verifying"

{-# COMPLETE
  TrustState_Created,
  TrustState_Creating,
  TrustState_Deleted,
  TrustState_Deleting,
  TrustState_Failed,
  TrustState_UpdateFailed,
  TrustState_Updated,
  TrustState_Updating,
  TrustState_Verified,
  TrustState_VerifyFailed,
  TrustState_Verifying,
  TrustState'
  #-}
