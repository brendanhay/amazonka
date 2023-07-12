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
-- Module      : Amazonka.RDS.Types.FailoverStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.FailoverStatus
  ( FailoverStatus
      ( ..,
        FailoverStatus_Cancelling,
        FailoverStatus_Failing_over,
        FailoverStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FailoverStatus = FailoverStatus'
  { fromFailoverStatus ::
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

pattern FailoverStatus_Cancelling :: FailoverStatus
pattern FailoverStatus_Cancelling = FailoverStatus' "cancelling"

pattern FailoverStatus_Failing_over :: FailoverStatus
pattern FailoverStatus_Failing_over = FailoverStatus' "failing-over"

pattern FailoverStatus_Pending :: FailoverStatus
pattern FailoverStatus_Pending = FailoverStatus' "pending"

{-# COMPLETE
  FailoverStatus_Cancelling,
  FailoverStatus_Failing_over,
  FailoverStatus_Pending,
  FailoverStatus'
  #-}
