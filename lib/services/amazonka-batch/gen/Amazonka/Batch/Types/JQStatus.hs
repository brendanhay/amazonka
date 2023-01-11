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
-- Module      : Amazonka.Batch.Types.JQStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.JQStatus
  ( JQStatus
      ( ..,
        JQStatus_CREATING,
        JQStatus_DELETED,
        JQStatus_DELETING,
        JQStatus_INVALID,
        JQStatus_UPDATING,
        JQStatus_VALID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype JQStatus = JQStatus'
  { fromJQStatus ::
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

pattern JQStatus_CREATING :: JQStatus
pattern JQStatus_CREATING = JQStatus' "CREATING"

pattern JQStatus_DELETED :: JQStatus
pattern JQStatus_DELETED = JQStatus' "DELETED"

pattern JQStatus_DELETING :: JQStatus
pattern JQStatus_DELETING = JQStatus' "DELETING"

pattern JQStatus_INVALID :: JQStatus
pattern JQStatus_INVALID = JQStatus' "INVALID"

pattern JQStatus_UPDATING :: JQStatus
pattern JQStatus_UPDATING = JQStatus' "UPDATING"

pattern JQStatus_VALID :: JQStatus
pattern JQStatus_VALID = JQStatus' "VALID"

{-# COMPLETE
  JQStatus_CREATING,
  JQStatus_DELETED,
  JQStatus_DELETING,
  JQStatus_INVALID,
  JQStatus_UPDATING,
  JQStatus_VALID,
  JQStatus'
  #-}
