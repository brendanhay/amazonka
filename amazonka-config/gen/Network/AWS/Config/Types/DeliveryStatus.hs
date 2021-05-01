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
-- Module      : Network.AWS.Config.Types.DeliveryStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.DeliveryStatus
  ( DeliveryStatus
      ( ..,
        DeliveryStatus_Failure,
        DeliveryStatus_Not_Applicable,
        DeliveryStatus_Success
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeliveryStatus = DeliveryStatus'
  { fromDeliveryStatus ::
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

pattern DeliveryStatus_Failure :: DeliveryStatus
pattern DeliveryStatus_Failure = DeliveryStatus' "Failure"

pattern DeliveryStatus_Not_Applicable :: DeliveryStatus
pattern DeliveryStatus_Not_Applicable = DeliveryStatus' "Not_Applicable"

pattern DeliveryStatus_Success :: DeliveryStatus
pattern DeliveryStatus_Success = DeliveryStatus' "Success"

{-# COMPLETE
  DeliveryStatus_Failure,
  DeliveryStatus_Not_Applicable,
  DeliveryStatus_Success,
  DeliveryStatus'
  #-}
