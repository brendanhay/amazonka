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
-- Module      : Amazonka.Outposts.Types.LineItemStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.LineItemStatus
  ( LineItemStatus
      ( ..,
        LineItemStatus_BUILDING,
        LineItemStatus_CANCELLED,
        LineItemStatus_DELIVERED,
        LineItemStatus_ERROR,
        LineItemStatus_INSTALLED,
        LineItemStatus_INSTALLING,
        LineItemStatus_PREPARING,
        LineItemStatus_SHIPPED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype LineItemStatus = LineItemStatus'
  { fromLineItemStatus ::
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

pattern LineItemStatus_BUILDING :: LineItemStatus
pattern LineItemStatus_BUILDING = LineItemStatus' "BUILDING"

pattern LineItemStatus_CANCELLED :: LineItemStatus
pattern LineItemStatus_CANCELLED = LineItemStatus' "CANCELLED"

pattern LineItemStatus_DELIVERED :: LineItemStatus
pattern LineItemStatus_DELIVERED = LineItemStatus' "DELIVERED"

pattern LineItemStatus_ERROR :: LineItemStatus
pattern LineItemStatus_ERROR = LineItemStatus' "ERROR"

pattern LineItemStatus_INSTALLED :: LineItemStatus
pattern LineItemStatus_INSTALLED = LineItemStatus' "INSTALLED"

pattern LineItemStatus_INSTALLING :: LineItemStatus
pattern LineItemStatus_INSTALLING = LineItemStatus' "INSTALLING"

pattern LineItemStatus_PREPARING :: LineItemStatus
pattern LineItemStatus_PREPARING = LineItemStatus' "PREPARING"

pattern LineItemStatus_SHIPPED :: LineItemStatus
pattern LineItemStatus_SHIPPED = LineItemStatus' "SHIPPED"

{-# COMPLETE
  LineItemStatus_BUILDING,
  LineItemStatus_CANCELLED,
  LineItemStatus_DELIVERED,
  LineItemStatus_ERROR,
  LineItemStatus_INSTALLED,
  LineItemStatus_INSTALLING,
  LineItemStatus_PREPARING,
  LineItemStatus_SHIPPED,
  LineItemStatus'
  #-}
