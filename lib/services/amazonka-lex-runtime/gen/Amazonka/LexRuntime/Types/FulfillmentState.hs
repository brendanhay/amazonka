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
-- Module      : Amazonka.LexRuntime.Types.FulfillmentState
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexRuntime.Types.FulfillmentState
  ( FulfillmentState
      ( ..,
        FulfillmentState_Failed,
        FulfillmentState_Fulfilled,
        FulfillmentState_ReadyForFulfillment
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype FulfillmentState = FulfillmentState'
  { fromFulfillmentState ::
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

pattern FulfillmentState_Failed :: FulfillmentState
pattern FulfillmentState_Failed = FulfillmentState' "Failed"

pattern FulfillmentState_Fulfilled :: FulfillmentState
pattern FulfillmentState_Fulfilled = FulfillmentState' "Fulfilled"

pattern FulfillmentState_ReadyForFulfillment :: FulfillmentState
pattern FulfillmentState_ReadyForFulfillment = FulfillmentState' "ReadyForFulfillment"

{-# COMPLETE
  FulfillmentState_Failed,
  FulfillmentState_Fulfilled,
  FulfillmentState_ReadyForFulfillment,
  FulfillmentState'
  #-}
