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
-- Module      : Network.AWS.EC2.Types.ListingState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ListingState
  ( ListingState
      ( ..,
        ListingState_Available,
        ListingState_Cancelled,
        ListingState_Pending,
        ListingState_Sold
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ListingState = ListingState'
  { fromListingState ::
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

pattern ListingState_Available :: ListingState
pattern ListingState_Available = ListingState' "available"

pattern ListingState_Cancelled :: ListingState
pattern ListingState_Cancelled = ListingState' "cancelled"

pattern ListingState_Pending :: ListingState
pattern ListingState_Pending = ListingState' "pending"

pattern ListingState_Sold :: ListingState
pattern ListingState_Sold = ListingState' "sold"

{-# COMPLETE
  ListingState_Available,
  ListingState_Cancelled,
  ListingState_Pending,
  ListingState_Sold,
  ListingState'
  #-}
