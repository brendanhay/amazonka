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
-- Module      : Amazonka.Batch.Types.CEStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Batch.Types.CEStatus
  ( CEStatus
      ( ..,
        CEStatus_CREATING,
        CEStatus_DELETED,
        CEStatus_DELETING,
        CEStatus_INVALID,
        CEStatus_UPDATING,
        CEStatus_VALID
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CEStatus = CEStatus'
  { fromCEStatus ::
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

pattern CEStatus_CREATING :: CEStatus
pattern CEStatus_CREATING = CEStatus' "CREATING"

pattern CEStatus_DELETED :: CEStatus
pattern CEStatus_DELETED = CEStatus' "DELETED"

pattern CEStatus_DELETING :: CEStatus
pattern CEStatus_DELETING = CEStatus' "DELETING"

pattern CEStatus_INVALID :: CEStatus
pattern CEStatus_INVALID = CEStatus' "INVALID"

pattern CEStatus_UPDATING :: CEStatus
pattern CEStatus_UPDATING = CEStatus' "UPDATING"

pattern CEStatus_VALID :: CEStatus
pattern CEStatus_VALID = CEStatus' "VALID"

{-# COMPLETE
  CEStatus_CREATING,
  CEStatus_DELETED,
  CEStatus_DELETING,
  CEStatus_INVALID,
  CEStatus_UPDATING,
  CEStatus_VALID,
  CEStatus'
  #-}
