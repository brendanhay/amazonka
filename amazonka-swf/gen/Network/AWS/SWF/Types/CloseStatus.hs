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
-- Module      : Network.AWS.SWF.Types.CloseStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.CloseStatus
  ( CloseStatus
      ( ..,
        CloseStatus_CANCELED,
        CloseStatus_COMPLETED,
        CloseStatus_CONTINUED_AS_NEW,
        CloseStatus_FAILED,
        CloseStatus_TERMINATED,
        CloseStatus_TIMED_OUT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype CloseStatus = CloseStatus'
  { fromCloseStatus ::
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

pattern CloseStatus_CANCELED :: CloseStatus
pattern CloseStatus_CANCELED = CloseStatus' "CANCELED"

pattern CloseStatus_COMPLETED :: CloseStatus
pattern CloseStatus_COMPLETED = CloseStatus' "COMPLETED"

pattern CloseStatus_CONTINUED_AS_NEW :: CloseStatus
pattern CloseStatus_CONTINUED_AS_NEW = CloseStatus' "CONTINUED_AS_NEW"

pattern CloseStatus_FAILED :: CloseStatus
pattern CloseStatus_FAILED = CloseStatus' "FAILED"

pattern CloseStatus_TERMINATED :: CloseStatus
pattern CloseStatus_TERMINATED = CloseStatus' "TERMINATED"

pattern CloseStatus_TIMED_OUT :: CloseStatus
pattern CloseStatus_TIMED_OUT = CloseStatus' "TIMED_OUT"

{-# COMPLETE
  CloseStatus_CANCELED,
  CloseStatus_COMPLETED,
  CloseStatus_CONTINUED_AS_NEW,
  CloseStatus_FAILED,
  CloseStatus_TERMINATED,
  CloseStatus_TIMED_OUT,
  CloseStatus'
  #-}
