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
-- Module      : Amazonka.MarketplaceCatalog.Types.ChangeStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceCatalog.Types.ChangeStatus
  ( ChangeStatus
      ( ..,
        ChangeStatus_APPLYING,
        ChangeStatus_CANCELLED,
        ChangeStatus_FAILED,
        ChangeStatus_PREPARING,
        ChangeStatus_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeStatus = ChangeStatus'
  { fromChangeStatus ::
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

pattern ChangeStatus_APPLYING :: ChangeStatus
pattern ChangeStatus_APPLYING = ChangeStatus' "APPLYING"

pattern ChangeStatus_CANCELLED :: ChangeStatus
pattern ChangeStatus_CANCELLED = ChangeStatus' "CANCELLED"

pattern ChangeStatus_FAILED :: ChangeStatus
pattern ChangeStatus_FAILED = ChangeStatus' "FAILED"

pattern ChangeStatus_PREPARING :: ChangeStatus
pattern ChangeStatus_PREPARING = ChangeStatus' "PREPARING"

pattern ChangeStatus_SUCCEEDED :: ChangeStatus
pattern ChangeStatus_SUCCEEDED = ChangeStatus' "SUCCEEDED"

{-# COMPLETE
  ChangeStatus_APPLYING,
  ChangeStatus_CANCELLED,
  ChangeStatus_FAILED,
  ChangeStatus_PREPARING,
  ChangeStatus_SUCCEEDED,
  ChangeStatus'
  #-}
