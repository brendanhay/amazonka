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
-- Module      : Amazonka.Amplify.Types.Platform
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Amplify.Types.Platform
  ( Platform
      ( ..,
        Platform_WEB,
        Platform_WEB_COMPUTE,
        Platform_WEB_DYNAMIC
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

pattern Platform_WEB :: Platform
pattern Platform_WEB = Platform' "WEB"

pattern Platform_WEB_COMPUTE :: Platform
pattern Platform_WEB_COMPUTE = Platform' "WEB_COMPUTE"

pattern Platform_WEB_DYNAMIC :: Platform
pattern Platform_WEB_DYNAMIC = Platform' "WEB_DYNAMIC"

{-# COMPLETE
  Platform_WEB,
  Platform_WEB_COMPUTE,
  Platform_WEB_DYNAMIC,
  Platform'
  #-}
