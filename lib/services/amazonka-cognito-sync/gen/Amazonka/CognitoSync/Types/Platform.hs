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
-- Module      : Amazonka.CognitoSync.Types.Platform
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoSync.Types.Platform
  ( Platform
      ( ..,
        Platform_ADM,
        Platform_APNS,
        Platform_APNS_SANDBOX,
        Platform_GCM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Platform = Platform'
  { fromPlatform ::
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

pattern Platform_ADM :: Platform
pattern Platform_ADM = Platform' "ADM"

pattern Platform_APNS :: Platform
pattern Platform_APNS = Platform' "APNS"

pattern Platform_APNS_SANDBOX :: Platform
pattern Platform_APNS_SANDBOX = Platform' "APNS_SANDBOX"

pattern Platform_GCM :: Platform
pattern Platform_GCM = Platform' "GCM"

{-# COMPLETE
  Platform_ADM,
  Platform_APNS,
  Platform_APNS_SANDBOX,
  Platform_GCM,
  Platform'
  #-}
