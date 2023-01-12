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
-- Module      : Amazonka.RAM.Types.ResourceShareStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RAM.Types.ResourceShareStatus
  ( ResourceShareStatus
      ( ..,
        ResourceShareStatus_ACTIVE,
        ResourceShareStatus_DELETED,
        ResourceShareStatus_DELETING,
        ResourceShareStatus_FAILED,
        ResourceShareStatus_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ResourceShareStatus = ResourceShareStatus'
  { fromResourceShareStatus ::
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

pattern ResourceShareStatus_ACTIVE :: ResourceShareStatus
pattern ResourceShareStatus_ACTIVE = ResourceShareStatus' "ACTIVE"

pattern ResourceShareStatus_DELETED :: ResourceShareStatus
pattern ResourceShareStatus_DELETED = ResourceShareStatus' "DELETED"

pattern ResourceShareStatus_DELETING :: ResourceShareStatus
pattern ResourceShareStatus_DELETING = ResourceShareStatus' "DELETING"

pattern ResourceShareStatus_FAILED :: ResourceShareStatus
pattern ResourceShareStatus_FAILED = ResourceShareStatus' "FAILED"

pattern ResourceShareStatus_PENDING :: ResourceShareStatus
pattern ResourceShareStatus_PENDING = ResourceShareStatus' "PENDING"

{-# COMPLETE
  ResourceShareStatus_ACTIVE,
  ResourceShareStatus_DELETED,
  ResourceShareStatus_DELETING,
  ResourceShareStatus_FAILED,
  ResourceShareStatus_PENDING,
  ResourceShareStatus'
  #-}
