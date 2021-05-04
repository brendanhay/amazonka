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
-- Module      : Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BlackoutSlateNetworkEndBlackout
  ( BlackoutSlateNetworkEndBlackout
      ( ..,
        BlackoutSlateNetworkEndBlackout_DISABLED,
        BlackoutSlateNetworkEndBlackout_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | Blackout Slate Network End Blackout
newtype BlackoutSlateNetworkEndBlackout = BlackoutSlateNetworkEndBlackout'
  { fromBlackoutSlateNetworkEndBlackout ::
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

pattern BlackoutSlateNetworkEndBlackout_DISABLED :: BlackoutSlateNetworkEndBlackout
pattern BlackoutSlateNetworkEndBlackout_DISABLED = BlackoutSlateNetworkEndBlackout' "DISABLED"

pattern BlackoutSlateNetworkEndBlackout_ENABLED :: BlackoutSlateNetworkEndBlackout
pattern BlackoutSlateNetworkEndBlackout_ENABLED = BlackoutSlateNetworkEndBlackout' "ENABLED"

{-# COMPLETE
  BlackoutSlateNetworkEndBlackout_DISABLED,
  BlackoutSlateNetworkEndBlackout_ENABLED,
  BlackoutSlateNetworkEndBlackout'
  #-}
