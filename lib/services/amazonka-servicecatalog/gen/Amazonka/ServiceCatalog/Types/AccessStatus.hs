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
-- Module      : Amazonka.ServiceCatalog.Types.AccessStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.AccessStatus
  ( AccessStatus
      ( ..,
        AccessStatus_DISABLED,
        AccessStatus_ENABLED,
        AccessStatus_UNDER_CHANGE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AccessStatus = AccessStatus'
  { fromAccessStatus ::
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

pattern AccessStatus_DISABLED :: AccessStatus
pattern AccessStatus_DISABLED = AccessStatus' "DISABLED"

pattern AccessStatus_ENABLED :: AccessStatus
pattern AccessStatus_ENABLED = AccessStatus' "ENABLED"

pattern AccessStatus_UNDER_CHANGE :: AccessStatus
pattern AccessStatus_UNDER_CHANGE = AccessStatus' "UNDER_CHANGE"

{-# COMPLETE
  AccessStatus_DISABLED,
  AccessStatus_ENABLED,
  AccessStatus_UNDER_CHANGE,
  AccessStatus'
  #-}
