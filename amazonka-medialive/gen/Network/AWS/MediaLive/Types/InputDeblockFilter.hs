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
-- Module      : Network.AWS.MediaLive.Types.InputDeblockFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeblockFilter
  ( InputDeblockFilter
      ( ..,
        InputDeblockFilter_DISABLED,
        InputDeblockFilter_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Input Deblock Filter
newtype InputDeblockFilter = InputDeblockFilter'
  { fromInputDeblockFilter ::
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

pattern InputDeblockFilter_DISABLED :: InputDeblockFilter
pattern InputDeblockFilter_DISABLED = InputDeblockFilter' "DISABLED"

pattern InputDeblockFilter_ENABLED :: InputDeblockFilter
pattern InputDeblockFilter_ENABLED = InputDeblockFilter' "ENABLED"

{-# COMPLETE
  InputDeblockFilter_DISABLED,
  InputDeblockFilter_ENABLED,
  InputDeblockFilter'
  #-}
