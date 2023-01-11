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
-- Module      : Amazonka.AuditManager.Types.ShareRequestStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ShareRequestStatus
  ( ShareRequestStatus
      ( ..,
        ShareRequestStatus_ACTIVE,
        ShareRequestStatus_DECLINED,
        ShareRequestStatus_EXPIRED,
        ShareRequestStatus_EXPIRING,
        ShareRequestStatus_FAILED,
        ShareRequestStatus_REPLICATING,
        ShareRequestStatus_REVOKED,
        ShareRequestStatus_SHARED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShareRequestStatus = ShareRequestStatus'
  { fromShareRequestStatus ::
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

pattern ShareRequestStatus_ACTIVE :: ShareRequestStatus
pattern ShareRequestStatus_ACTIVE = ShareRequestStatus' "ACTIVE"

pattern ShareRequestStatus_DECLINED :: ShareRequestStatus
pattern ShareRequestStatus_DECLINED = ShareRequestStatus' "DECLINED"

pattern ShareRequestStatus_EXPIRED :: ShareRequestStatus
pattern ShareRequestStatus_EXPIRED = ShareRequestStatus' "EXPIRED"

pattern ShareRequestStatus_EXPIRING :: ShareRequestStatus
pattern ShareRequestStatus_EXPIRING = ShareRequestStatus' "EXPIRING"

pattern ShareRequestStatus_FAILED :: ShareRequestStatus
pattern ShareRequestStatus_FAILED = ShareRequestStatus' "FAILED"

pattern ShareRequestStatus_REPLICATING :: ShareRequestStatus
pattern ShareRequestStatus_REPLICATING = ShareRequestStatus' "REPLICATING"

pattern ShareRequestStatus_REVOKED :: ShareRequestStatus
pattern ShareRequestStatus_REVOKED = ShareRequestStatus' "REVOKED"

pattern ShareRequestStatus_SHARED :: ShareRequestStatus
pattern ShareRequestStatus_SHARED = ShareRequestStatus' "SHARED"

{-# COMPLETE
  ShareRequestStatus_ACTIVE,
  ShareRequestStatus_DECLINED,
  ShareRequestStatus_EXPIRED,
  ShareRequestStatus_EXPIRING,
  ShareRequestStatus_FAILED,
  ShareRequestStatus_REPLICATING,
  ShareRequestStatus_REVOKED,
  ShareRequestStatus_SHARED,
  ShareRequestStatus'
  #-}
