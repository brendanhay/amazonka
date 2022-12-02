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
-- Module      : Amazonka.StorageGateway.Types.RetentionLockType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.RetentionLockType
  ( RetentionLockType
      ( ..,
        RetentionLockType_COMPLIANCE,
        RetentionLockType_GOVERNANCE,
        RetentionLockType_NONE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RetentionLockType = RetentionLockType'
  { fromRetentionLockType ::
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

pattern RetentionLockType_COMPLIANCE :: RetentionLockType
pattern RetentionLockType_COMPLIANCE = RetentionLockType' "COMPLIANCE"

pattern RetentionLockType_GOVERNANCE :: RetentionLockType
pattern RetentionLockType_GOVERNANCE = RetentionLockType' "GOVERNANCE"

pattern RetentionLockType_NONE :: RetentionLockType
pattern RetentionLockType_NONE = RetentionLockType' "NONE"

{-# COMPLETE
  RetentionLockType_COMPLIANCE,
  RetentionLockType_GOVERNANCE,
  RetentionLockType_NONE,
  RetentionLockType'
  #-}
