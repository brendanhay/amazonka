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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetStatus
  ( ChangeSetStatus
      ( ..,
        ChangeSetStatus_CREATE_COMPLETE,
        ChangeSetStatus_CREATE_IN_PROGRESS,
        ChangeSetStatus_CREATE_PENDING,
        ChangeSetStatus_DELETE_COMPLETE,
        ChangeSetStatus_DELETE_FAILED,
        ChangeSetStatus_DELETE_IN_PROGRESS,
        ChangeSetStatus_DELETE_PENDING,
        ChangeSetStatus_FAILED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeSetStatus = ChangeSetStatus'
  { fromChangeSetStatus ::
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

pattern ChangeSetStatus_CREATE_COMPLETE :: ChangeSetStatus
pattern ChangeSetStatus_CREATE_COMPLETE = ChangeSetStatus' "CREATE_COMPLETE"

pattern ChangeSetStatus_CREATE_IN_PROGRESS :: ChangeSetStatus
pattern ChangeSetStatus_CREATE_IN_PROGRESS = ChangeSetStatus' "CREATE_IN_PROGRESS"

pattern ChangeSetStatus_CREATE_PENDING :: ChangeSetStatus
pattern ChangeSetStatus_CREATE_PENDING = ChangeSetStatus' "CREATE_PENDING"

pattern ChangeSetStatus_DELETE_COMPLETE :: ChangeSetStatus
pattern ChangeSetStatus_DELETE_COMPLETE = ChangeSetStatus' "DELETE_COMPLETE"

pattern ChangeSetStatus_DELETE_FAILED :: ChangeSetStatus
pattern ChangeSetStatus_DELETE_FAILED = ChangeSetStatus' "DELETE_FAILED"

pattern ChangeSetStatus_DELETE_IN_PROGRESS :: ChangeSetStatus
pattern ChangeSetStatus_DELETE_IN_PROGRESS = ChangeSetStatus' "DELETE_IN_PROGRESS"

pattern ChangeSetStatus_DELETE_PENDING :: ChangeSetStatus
pattern ChangeSetStatus_DELETE_PENDING = ChangeSetStatus' "DELETE_PENDING"

pattern ChangeSetStatus_FAILED :: ChangeSetStatus
pattern ChangeSetStatus_FAILED = ChangeSetStatus' "FAILED"

{-# COMPLETE
  ChangeSetStatus_CREATE_COMPLETE,
  ChangeSetStatus_CREATE_IN_PROGRESS,
  ChangeSetStatus_CREATE_PENDING,
  ChangeSetStatus_DELETE_COMPLETE,
  ChangeSetStatus_DELETE_FAILED,
  ChangeSetStatus_DELETE_IN_PROGRESS,
  ChangeSetStatus_DELETE_PENDING,
  ChangeSetStatus_FAILED,
  ChangeSetStatus'
  #-}
