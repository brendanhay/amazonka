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
-- Module      : Amazonka.IoTEvents.Types.InputStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.InputStatus
  ( InputStatus
      ( ..,
        InputStatus_ACTIVE,
        InputStatus_CREATING,
        InputStatus_DELETING,
        InputStatus_UPDATING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InputStatus = InputStatus'
  { fromInputStatus ::
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

pattern InputStatus_ACTIVE :: InputStatus
pattern InputStatus_ACTIVE = InputStatus' "ACTIVE"

pattern InputStatus_CREATING :: InputStatus
pattern InputStatus_CREATING = InputStatus' "CREATING"

pattern InputStatus_DELETING :: InputStatus
pattern InputStatus_DELETING = InputStatus' "DELETING"

pattern InputStatus_UPDATING :: InputStatus
pattern InputStatus_UPDATING = InputStatus' "UPDATING"

{-# COMPLETE
  InputStatus_ACTIVE,
  InputStatus_CREATING,
  InputStatus_DELETING,
  InputStatus_UPDATING,
  InputStatus'
  #-}
