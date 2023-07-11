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
-- Module      : Amazonka.Connect.Types.InstanceStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceStatus
  ( InstanceStatus
      ( ..,
        InstanceStatus_ACTIVE,
        InstanceStatus_CREATION_FAILED,
        InstanceStatus_CREATION_IN_PROGRESS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InstanceStatus = InstanceStatus'
  { fromInstanceStatus ::
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

pattern InstanceStatus_ACTIVE :: InstanceStatus
pattern InstanceStatus_ACTIVE = InstanceStatus' "ACTIVE"

pattern InstanceStatus_CREATION_FAILED :: InstanceStatus
pattern InstanceStatus_CREATION_FAILED = InstanceStatus' "CREATION_FAILED"

pattern InstanceStatus_CREATION_IN_PROGRESS :: InstanceStatus
pattern InstanceStatus_CREATION_IN_PROGRESS = InstanceStatus' "CREATION_IN_PROGRESS"

{-# COMPLETE
  InstanceStatus_ACTIVE,
  InstanceStatus_CREATION_FAILED,
  InstanceStatus_CREATION_IN_PROGRESS,
  InstanceStatus'
  #-}
