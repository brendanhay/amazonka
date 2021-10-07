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
-- Module      : Network.AWS.EC2.Types.CancelSpotInstanceRequestState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CancelSpotInstanceRequestState
  ( CancelSpotInstanceRequestState
      ( ..,
        CancelSpotInstanceRequestState_Active,
        CancelSpotInstanceRequestState_Cancelled,
        CancelSpotInstanceRequestState_Closed,
        CancelSpotInstanceRequestState_Completed,
        CancelSpotInstanceRequestState_Open
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype CancelSpotInstanceRequestState = CancelSpotInstanceRequestState'
  { fromCancelSpotInstanceRequestState ::
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

pattern CancelSpotInstanceRequestState_Active :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestState_Active = CancelSpotInstanceRequestState' "active"

pattern CancelSpotInstanceRequestState_Cancelled :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestState_Cancelled = CancelSpotInstanceRequestState' "cancelled"

pattern CancelSpotInstanceRequestState_Closed :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestState_Closed = CancelSpotInstanceRequestState' "closed"

pattern CancelSpotInstanceRequestState_Completed :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestState_Completed = CancelSpotInstanceRequestState' "completed"

pattern CancelSpotInstanceRequestState_Open :: CancelSpotInstanceRequestState
pattern CancelSpotInstanceRequestState_Open = CancelSpotInstanceRequestState' "open"

{-# COMPLETE
  CancelSpotInstanceRequestState_Active,
  CancelSpotInstanceRequestState_Cancelled,
  CancelSpotInstanceRequestState_Closed,
  CancelSpotInstanceRequestState_Completed,
  CancelSpotInstanceRequestState_Open,
  CancelSpotInstanceRequestState'
  #-}
