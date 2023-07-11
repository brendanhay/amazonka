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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ConversionTaskState = ConversionTaskState'
  { fromConversionTaskState ::
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
