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
-- Module      : Amazonka.EC2.Types.ListingStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ListingStatus
  ( ListingStatus
      ( ..,
        ListingStatus_Active,
        ListingStatus_Cancelled,
        ListingStatus_Closed,
        ListingStatus_Pending
      ),
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ListingStatus = ListingStatus'
  { fromListingStatus ::
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

pattern ListingStatus_Active :: ListingStatus
pattern ListingStatus_Active = ListingStatus' "active"

pattern ListingStatus_Cancelled :: ListingStatus
pattern ListingStatus_Cancelled = ListingStatus' "cancelled"

pattern ListingStatus_Closed :: ListingStatus
pattern ListingStatus_Closed = ListingStatus' "closed"

pattern ListingStatus_Pending :: ListingStatus
pattern ListingStatus_Pending = ListingStatus' "pending"

{-# COMPLETE
  ListingStatus_Active,
  ListingStatus_Cancelled,
  ListingStatus_Closed,
  ListingStatus_Pending,
  ListingStatus'
  #-}
