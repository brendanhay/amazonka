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
-- Module      : Amazonka.RDS.Types.CustomEngineVersionStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.CustomEngineVersionStatus
  ( CustomEngineVersionStatus
      ( ..,
        CustomEngineVersionStatus_Available,
        CustomEngineVersionStatus_Inactive,
        CustomEngineVersionStatus_Inactive_except_restore
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype CustomEngineVersionStatus = CustomEngineVersionStatus'
  { fromCustomEngineVersionStatus ::
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

pattern CustomEngineVersionStatus_Available :: CustomEngineVersionStatus
pattern CustomEngineVersionStatus_Available = CustomEngineVersionStatus' "available"

pattern CustomEngineVersionStatus_Inactive :: CustomEngineVersionStatus
pattern CustomEngineVersionStatus_Inactive = CustomEngineVersionStatus' "inactive"

pattern CustomEngineVersionStatus_Inactive_except_restore :: CustomEngineVersionStatus
pattern CustomEngineVersionStatus_Inactive_except_restore = CustomEngineVersionStatus' "inactive-except-restore"

{-# COMPLETE
  CustomEngineVersionStatus_Available,
  CustomEngineVersionStatus_Inactive,
  CustomEngineVersionStatus_Inactive_except_restore,
  CustomEngineVersionStatus'
  #-}
