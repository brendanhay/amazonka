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
-- Module      : Amazonka.OsIs.Types.ChangeProgressStatuses
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OsIs.Types.ChangeProgressStatuses
  ( ChangeProgressStatuses
      ( ..,
        ChangeProgressStatuses_COMPLETED,
        ChangeProgressStatuses_FAILED,
        ChangeProgressStatuses_IN_PROGRESS,
        ChangeProgressStatuses_PENDING
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeProgressStatuses = ChangeProgressStatuses'
  { fromChangeProgressStatuses ::
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

pattern ChangeProgressStatuses_COMPLETED :: ChangeProgressStatuses
pattern ChangeProgressStatuses_COMPLETED = ChangeProgressStatuses' "COMPLETED"

pattern ChangeProgressStatuses_FAILED :: ChangeProgressStatuses
pattern ChangeProgressStatuses_FAILED = ChangeProgressStatuses' "FAILED"

pattern ChangeProgressStatuses_IN_PROGRESS :: ChangeProgressStatuses
pattern ChangeProgressStatuses_IN_PROGRESS = ChangeProgressStatuses' "IN_PROGRESS"

pattern ChangeProgressStatuses_PENDING :: ChangeProgressStatuses
pattern ChangeProgressStatuses_PENDING = ChangeProgressStatuses' "PENDING"

{-# COMPLETE
  ChangeProgressStatuses_COMPLETED,
  ChangeProgressStatuses_FAILED,
  ChangeProgressStatuses_IN_PROGRESS,
  ChangeProgressStatuses_PENDING,
  ChangeProgressStatuses'
  #-}
