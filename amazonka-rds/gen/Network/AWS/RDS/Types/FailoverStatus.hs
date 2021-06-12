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
-- Module      : Network.AWS.RDS.Types.FailoverStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.FailoverStatus
  ( FailoverStatus
      ( ..,
        FailoverStatus_Cancelling,
        FailoverStatus_Failing_over,
        FailoverStatus_Pending
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype FailoverStatus = FailoverStatus'
  { fromFailoverStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
