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
-- Module      : Network.AWS.ApiGatewayV2.Types.ContentHandlingStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ApiGatewayV2.Types.ContentHandlingStrategy
  ( ContentHandlingStrategy
      ( ..,
        ContentHandlingStrategy_CONVERT_TO_BINARY,
        ContentHandlingStrategy_CONVERT_TO_TEXT
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specifies how to handle response payload content type conversions.
-- Supported only for WebSocket APIs.
newtype ContentHandlingStrategy = ContentHandlingStrategy'
  { fromContentHandlingStrategy ::
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

pattern ContentHandlingStrategy_CONVERT_TO_BINARY :: ContentHandlingStrategy
pattern ContentHandlingStrategy_CONVERT_TO_BINARY = ContentHandlingStrategy' "CONVERT_TO_BINARY"

pattern ContentHandlingStrategy_CONVERT_TO_TEXT :: ContentHandlingStrategy
pattern ContentHandlingStrategy_CONVERT_TO_TEXT = ContentHandlingStrategy' "CONVERT_TO_TEXT"

{-# COMPLETE
  ContentHandlingStrategy_CONVERT_TO_BINARY,
  ContentHandlingStrategy_CONVERT_TO_TEXT,
  ContentHandlingStrategy'
  #-}
