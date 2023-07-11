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
-- Module      : Amazonka.WellArchitected.Types.ShareStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_ACCEPTED,
        ShareStatus_ASSOCIATED,
        ShareStatus_ASSOCIATING,
        ShareStatus_EXPIRED,
        ShareStatus_FAILED,
        ShareStatus_PENDING,
        ShareStatus_REJECTED,
        ShareStatus_REVOKED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The status of a workload share.
newtype ShareStatus = ShareStatus'
  { fromShareStatus ::
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

pattern ShareStatus_ACCEPTED :: ShareStatus
pattern ShareStatus_ACCEPTED = ShareStatus' "ACCEPTED"

pattern ShareStatus_ASSOCIATED :: ShareStatus
pattern ShareStatus_ASSOCIATED = ShareStatus' "ASSOCIATED"

pattern ShareStatus_ASSOCIATING :: ShareStatus
pattern ShareStatus_ASSOCIATING = ShareStatus' "ASSOCIATING"

pattern ShareStatus_EXPIRED :: ShareStatus
pattern ShareStatus_EXPIRED = ShareStatus' "EXPIRED"

pattern ShareStatus_FAILED :: ShareStatus
pattern ShareStatus_FAILED = ShareStatus' "FAILED"

pattern ShareStatus_PENDING :: ShareStatus
pattern ShareStatus_PENDING = ShareStatus' "PENDING"

pattern ShareStatus_REJECTED :: ShareStatus
pattern ShareStatus_REJECTED = ShareStatus' "REJECTED"

pattern ShareStatus_REVOKED :: ShareStatus
pattern ShareStatus_REVOKED = ShareStatus' "REVOKED"

{-# COMPLETE
  ShareStatus_ACCEPTED,
  ShareStatus_ASSOCIATED,
  ShareStatus_ASSOCIATING,
  ShareStatus_EXPIRED,
  ShareStatus_FAILED,
  ShareStatus_PENDING,
  ShareStatus_REJECTED,
  ShareStatus_REVOKED,
  ShareStatus'
  #-}
