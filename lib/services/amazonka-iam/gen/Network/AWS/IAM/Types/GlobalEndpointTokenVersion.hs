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
-- Module      : Amazonka.IAM.Types.GlobalEndpointTokenVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.GlobalEndpointTokenVersion
  ( GlobalEndpointTokenVersion
      ( ..,
        GlobalEndpointTokenVersion_V1Token,
        GlobalEndpointTokenVersion_V2Token
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype GlobalEndpointTokenVersion = GlobalEndpointTokenVersion'
  { fromGlobalEndpointTokenVersion ::
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

pattern GlobalEndpointTokenVersion_V1Token :: GlobalEndpointTokenVersion
pattern GlobalEndpointTokenVersion_V1Token = GlobalEndpointTokenVersion' "v1Token"

pattern GlobalEndpointTokenVersion_V2Token :: GlobalEndpointTokenVersion
pattern GlobalEndpointTokenVersion_V2Token = GlobalEndpointTokenVersion' "v2Token"

{-# COMPLETE
  GlobalEndpointTokenVersion_V1Token,
  GlobalEndpointTokenVersion_V2Token,
  GlobalEndpointTokenVersion'
  #-}
