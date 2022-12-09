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
-- Module      : Amazonka.CloudWatchLogs.Types.DataProtectionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.DataProtectionStatus
  ( DataProtectionStatus
      ( ..,
        DataProtectionStatus_ACTIVATED,
        DataProtectionStatus_ARCHIVED,
        DataProtectionStatus_DELETED,
        DataProtectionStatus_DISABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DataProtectionStatus = DataProtectionStatus'
  { fromDataProtectionStatus ::
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

pattern DataProtectionStatus_ACTIVATED :: DataProtectionStatus
pattern DataProtectionStatus_ACTIVATED = DataProtectionStatus' "ACTIVATED"

pattern DataProtectionStatus_ARCHIVED :: DataProtectionStatus
pattern DataProtectionStatus_ARCHIVED = DataProtectionStatus' "ARCHIVED"

pattern DataProtectionStatus_DELETED :: DataProtectionStatus
pattern DataProtectionStatus_DELETED = DataProtectionStatus' "DELETED"

pattern DataProtectionStatus_DISABLED :: DataProtectionStatus
pattern DataProtectionStatus_DISABLED = DataProtectionStatus' "DISABLED"

{-# COMPLETE
  DataProtectionStatus_ACTIVATED,
  DataProtectionStatus_ARCHIVED,
  DataProtectionStatus_DELETED,
  DataProtectionStatus_DISABLED,
  DataProtectionStatus'
  #-}
