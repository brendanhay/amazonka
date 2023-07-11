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
-- Module      : Amazonka.ManagedBlockChain.Types.AccessorStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ManagedBlockChain.Types.AccessorStatus
  ( AccessorStatus
      ( ..,
        AccessorStatus_AVAILABLE,
        AccessorStatus_DELETED,
        AccessorStatus_PENDING_DELETION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccessorStatus = AccessorStatus'
  { fromAccessorStatus ::
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

pattern AccessorStatus_AVAILABLE :: AccessorStatus
pattern AccessorStatus_AVAILABLE = AccessorStatus' "AVAILABLE"

pattern AccessorStatus_DELETED :: AccessorStatus
pattern AccessorStatus_DELETED = AccessorStatus' "DELETED"

pattern AccessorStatus_PENDING_DELETION :: AccessorStatus
pattern AccessorStatus_PENDING_DELETION = AccessorStatus' "PENDING_DELETION"

{-# COMPLETE
  AccessorStatus_AVAILABLE,
  AccessorStatus_DELETED,
  AccessorStatus_PENDING_DELETION,
  AccessorStatus'
  #-}
