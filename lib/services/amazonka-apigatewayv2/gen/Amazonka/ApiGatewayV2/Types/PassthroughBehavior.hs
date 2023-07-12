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
-- Module      : Amazonka.ApiGatewayV2.Types.PassthroughBehavior
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.PassthroughBehavior
  ( PassthroughBehavior
      ( ..,
        PassthroughBehavior_NEVER,
        PassthroughBehavior_WHEN_NO_MATCH,
        PassthroughBehavior_WHEN_NO_TEMPLATES
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents passthrough behavior for an integration response. Supported
-- only for WebSocket APIs.
newtype PassthroughBehavior = PassthroughBehavior'
  { fromPassthroughBehavior ::
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

pattern PassthroughBehavior_NEVER :: PassthroughBehavior
pattern PassthroughBehavior_NEVER = PassthroughBehavior' "NEVER"

pattern PassthroughBehavior_WHEN_NO_MATCH :: PassthroughBehavior
pattern PassthroughBehavior_WHEN_NO_MATCH = PassthroughBehavior' "WHEN_NO_MATCH"

pattern PassthroughBehavior_WHEN_NO_TEMPLATES :: PassthroughBehavior
pattern PassthroughBehavior_WHEN_NO_TEMPLATES = PassthroughBehavior' "WHEN_NO_TEMPLATES"

{-# COMPLETE
  PassthroughBehavior_NEVER,
  PassthroughBehavior_WHEN_NO_MATCH,
  PassthroughBehavior_WHEN_NO_TEMPLATES,
  PassthroughBehavior'
  #-}
