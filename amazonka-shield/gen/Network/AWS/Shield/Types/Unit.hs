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
-- Module      : Network.AWS.Shield.Types.Unit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Unit
  ( Unit
      ( ..,
        Unit_BITS,
        Unit_BYTES,
        Unit_PACKETS,
        Unit_REQUESTS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Unit = Unit' {fromUnit :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern Unit_BITS :: Unit
pattern Unit_BITS = Unit' "BITS"

pattern Unit_BYTES :: Unit
pattern Unit_BYTES = Unit' "BYTES"

pattern Unit_PACKETS :: Unit
pattern Unit_PACKETS = Unit' "PACKETS"

pattern Unit_REQUESTS :: Unit
pattern Unit_REQUESTS = Unit' "REQUESTS"

{-# COMPLETE
  Unit_BITS,
  Unit_BYTES,
  Unit_PACKETS,
  Unit_REQUESTS,
  Unit'
  #-}
