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
-- Module      : Amazonka.CloudFormation.Types.StackDriftStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackDriftStatus
  ( StackDriftStatus
      ( ..,
        StackDriftStatus_DRIFTED,
        StackDriftStatus_IN_SYNC,
        StackDriftStatus_NOT_CHECKED,
        StackDriftStatus_UNKNOWN
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackDriftStatus = StackDriftStatus'
  { fromStackDriftStatus ::
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

pattern StackDriftStatus_DRIFTED :: StackDriftStatus
pattern StackDriftStatus_DRIFTED = StackDriftStatus' "DRIFTED"

pattern StackDriftStatus_IN_SYNC :: StackDriftStatus
pattern StackDriftStatus_IN_SYNC = StackDriftStatus' "IN_SYNC"

pattern StackDriftStatus_NOT_CHECKED :: StackDriftStatus
pattern StackDriftStatus_NOT_CHECKED = StackDriftStatus' "NOT_CHECKED"

pattern StackDriftStatus_UNKNOWN :: StackDriftStatus
pattern StackDriftStatus_UNKNOWN = StackDriftStatus' "UNKNOWN"

{-# COMPLETE
  StackDriftStatus_DRIFTED,
  StackDriftStatus_IN_SYNC,
  StackDriftStatus_NOT_CHECKED,
  StackDriftStatus_UNKNOWN,
  StackDriftStatus'
  #-}
