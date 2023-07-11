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
-- Module      : Amazonka.Snowball.Types.ShippingLabelStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Snowball.Types.ShippingLabelStatus
  ( ShippingLabelStatus
      ( ..,
        ShippingLabelStatus_Failed,
        ShippingLabelStatus_InProgress,
        ShippingLabelStatus_Succeeded,
        ShippingLabelStatus_TimedOut
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ShippingLabelStatus = ShippingLabelStatus'
  { fromShippingLabelStatus ::
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

pattern ShippingLabelStatus_Failed :: ShippingLabelStatus
pattern ShippingLabelStatus_Failed = ShippingLabelStatus' "Failed"

pattern ShippingLabelStatus_InProgress :: ShippingLabelStatus
pattern ShippingLabelStatus_InProgress = ShippingLabelStatus' "InProgress"

pattern ShippingLabelStatus_Succeeded :: ShippingLabelStatus
pattern ShippingLabelStatus_Succeeded = ShippingLabelStatus' "Succeeded"

pattern ShippingLabelStatus_TimedOut :: ShippingLabelStatus
pattern ShippingLabelStatus_TimedOut = ShippingLabelStatus' "TimedOut"

{-# COMPLETE
  ShippingLabelStatus_Failed,
  ShippingLabelStatus_InProgress,
  ShippingLabelStatus_Succeeded,
  ShippingLabelStatus_TimedOut,
  ShippingLabelStatus'
  #-}
