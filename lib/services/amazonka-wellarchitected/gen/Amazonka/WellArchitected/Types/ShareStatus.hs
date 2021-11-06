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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ShareStatus
  ( ShareStatus
      ( ..,
        ShareStatus_ACCEPTED,
        ShareStatus_EXPIRED,
        ShareStatus_PENDING,
        ShareStatus_REJECTED,
        ShareStatus_REVOKED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | The status of a workload share.
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

pattern ShareStatus_ACCEPTED :: ShareStatus
pattern ShareStatus_ACCEPTED = ShareStatus' "ACCEPTED"

pattern ShareStatus_EXPIRED :: ShareStatus
pattern ShareStatus_EXPIRED = ShareStatus' "EXPIRED"

pattern ShareStatus_PENDING :: ShareStatus
pattern ShareStatus_PENDING = ShareStatus' "PENDING"

pattern ShareStatus_REJECTED :: ShareStatus
pattern ShareStatus_REJECTED = ShareStatus' "REJECTED"

pattern ShareStatus_REVOKED :: ShareStatus
pattern ShareStatus_REVOKED = ShareStatus' "REVOKED"

{-# COMPLETE
  ShareStatus_ACCEPTED,
  ShareStatus_EXPIRED,
  ShareStatus_PENDING,
  ShareStatus_REJECTED,
  ShareStatus_REVOKED,
  ShareStatus'
  #-}
