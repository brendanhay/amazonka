{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
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

import qualified Network.AWS.Prelude as Prelude

-- | Specifies whether the input device has been configured (outside of
-- MediaLive) to use a dynamic IP address assignment (DHCP) or a static IP
-- address.
newtype InputDeviceIpScheme = InputDeviceIpScheme'
  { fromInputDeviceIpScheme ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
