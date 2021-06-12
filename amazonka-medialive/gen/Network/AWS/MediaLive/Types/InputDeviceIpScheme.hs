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
-- Module      : Network.AWS.MediaLive.Types.InputDeviceIpScheme
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceIpScheme
  ( InputDeviceIpScheme
      ( ..,
        InputDeviceIpScheme_DHCP,
        InputDeviceIpScheme_STATIC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
newtype InputDeviceIpScheme = InputDeviceIpScheme'
  { fromInputDeviceIpScheme ::
      Core.Text
  }
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

pattern InputDeviceIpScheme_DHCP :: InputDeviceIpScheme
pattern InputDeviceIpScheme_DHCP = InputDeviceIpScheme' "DHCP"

pattern InputDeviceIpScheme_STATIC :: InputDeviceIpScheme
pattern InputDeviceIpScheme_STATIC = InputDeviceIpScheme' "STATIC"

{-# COMPLETE
  InputDeviceIpScheme_DHCP,
  InputDeviceIpScheme_STATIC,
  InputDeviceIpScheme'
  #-}
