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
-- Module      : Network.AWS.ApiGatewayV2.Types.PassthroughBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.PassthroughBehavior
  ( PassthroughBehavior
      ( ..,
        PassthroughBehavior_NEVER,
        PassthroughBehavior_WHEN_NO_MATCH,
        PassthroughBehavior_WHEN_NO_TEMPLATES
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Represents passthrough behavior for an integration response. Supported
-- only for WebSocket APIs.
newtype PassthroughBehavior = PassthroughBehavior'
  { fromPassthroughBehavior ::
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
