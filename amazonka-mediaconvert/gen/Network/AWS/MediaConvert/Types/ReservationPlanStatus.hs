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
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanStatus
  ( ReservationPlanStatus
      ( ..,
        ReservationPlanStatus_ACTIVE,
        ReservationPlanStatus_EXPIRED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or
-- EXPIRED.
newtype ReservationPlanStatus = ReservationPlanStatus'
  { fromReservationPlanStatus ::
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

pattern ReservationPlanStatus_ACTIVE :: ReservationPlanStatus
pattern ReservationPlanStatus_ACTIVE = ReservationPlanStatus' "ACTIVE"

pattern ReservationPlanStatus_EXPIRED :: ReservationPlanStatus
pattern ReservationPlanStatus_EXPIRED = ReservationPlanStatus' "EXPIRED"

{-# COMPLETE
  ReservationPlanStatus_ACTIVE,
  ReservationPlanStatus_EXPIRED,
  ReservationPlanStatus'
  #-}
