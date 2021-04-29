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

import qualified Network.AWS.Prelude as Prelude

newtype CloseStatus = CloseStatus'
  { fromCloseStatus ::
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
