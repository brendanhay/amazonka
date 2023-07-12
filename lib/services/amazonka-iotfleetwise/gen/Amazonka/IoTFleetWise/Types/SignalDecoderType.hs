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
-- Module      : Amazonka.IoTFleetWise.Types.SignalDecoderType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTFleetWise.Types.SignalDecoderType
  ( SignalDecoderType
      ( ..,
        SignalDecoderType_CAN_SIGNAL,
        SignalDecoderType_OBD_SIGNAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SignalDecoderType = SignalDecoderType'
  { fromSignalDecoderType ::
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

pattern SignalDecoderType_CAN_SIGNAL :: SignalDecoderType
pattern SignalDecoderType_CAN_SIGNAL = SignalDecoderType' "CAN_SIGNAL"

pattern SignalDecoderType_OBD_SIGNAL :: SignalDecoderType
pattern SignalDecoderType_OBD_SIGNAL = SignalDecoderType' "OBD_SIGNAL"

{-# COMPLETE
  SignalDecoderType_CAN_SIGNAL,
  SignalDecoderType_OBD_SIGNAL,
  SignalDecoderType'
  #-}
