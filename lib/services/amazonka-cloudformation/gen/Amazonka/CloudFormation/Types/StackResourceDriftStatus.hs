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
-- Module      : Amazonka.CloudFormation.Types.StackResourceDriftStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackResourceDriftStatus
  ( StackResourceDriftStatus
      ( ..,
        StackResourceDriftStatus_DELETED,
        StackResourceDriftStatus_IN_SYNC,
        StackResourceDriftStatus_MODIFIED,
        StackResourceDriftStatus_NOT_CHECKED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype StackResourceDriftStatus = StackResourceDriftStatus'
  { fromStackResourceDriftStatus ::
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

pattern StackResourceDriftStatus_DELETED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_DELETED = StackResourceDriftStatus' "DELETED"

pattern StackResourceDriftStatus_IN_SYNC :: StackResourceDriftStatus
pattern StackResourceDriftStatus_IN_SYNC = StackResourceDriftStatus' "IN_SYNC"

pattern StackResourceDriftStatus_MODIFIED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_MODIFIED = StackResourceDriftStatus' "MODIFIED"

pattern StackResourceDriftStatus_NOT_CHECKED :: StackResourceDriftStatus
pattern StackResourceDriftStatus_NOT_CHECKED = StackResourceDriftStatus' "NOT_CHECKED"

{-# COMPLETE
  StackResourceDriftStatus_DELETED,
  StackResourceDriftStatus_IN_SYNC,
  StackResourceDriftStatus_MODIFIED,
  StackResourceDriftStatus_NOT_CHECKED,
  StackResourceDriftStatus'
  #-}
