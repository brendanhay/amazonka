{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

newtype FailoverStatus = FailoverStatus'
  { fromFailoverStatus ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
