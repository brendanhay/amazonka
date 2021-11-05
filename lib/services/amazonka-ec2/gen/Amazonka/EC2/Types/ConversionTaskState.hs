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
-- Module      : Amazonka.EC2.Types.ConversionTaskState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ConversionTaskState
  ( ConversionTaskState
      ( ..,
        ConversionTaskState_Active,
        ConversionTaskState_Cancelled,
        ConversionTaskState_Cancelling,
        ConversionTaskState_Completed
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ConversionTaskState = ConversionTaskState'
  { fromConversionTaskState ::
      Core.Text
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

pattern ConversionTaskState_Active :: ConversionTaskState
pattern ConversionTaskState_Active = ConversionTaskState' "active"

pattern ConversionTaskState_Cancelled :: ConversionTaskState
pattern ConversionTaskState_Cancelled = ConversionTaskState' "cancelled"

pattern ConversionTaskState_Cancelling :: ConversionTaskState
pattern ConversionTaskState_Cancelling = ConversionTaskState' "cancelling"

pattern ConversionTaskState_Completed :: ConversionTaskState
pattern ConversionTaskState_Completed = ConversionTaskState' "completed"

{-# COMPLETE
  ConversionTaskState_Active,
  ConversionTaskState_Cancelled,
  ConversionTaskState_Cancelling,
  ConversionTaskState_Completed,
  ConversionTaskState'
  #-}
