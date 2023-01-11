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
-- Module      : Amazonka.DirectoryService.Types.TrustState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.TrustState
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype TrustState = TrustState'
  { fromTrustState ::
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
