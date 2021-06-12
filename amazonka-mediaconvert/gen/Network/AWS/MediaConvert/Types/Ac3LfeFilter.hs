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
-- Module      : Network.AWS.MediaConvert.Types.Ac3LfeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.Ac3LfeFilter
  ( Ac3LfeFilter
      ( ..,
        Ac3LfeFilter_DISABLED,
        Ac3LfeFilter_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Applies a 120Hz lowpass filter to the LFE channel prior to encoding.
-- Only valid with 3_2_LFE coding mode.
newtype Ac3LfeFilter = Ac3LfeFilter'
  { fromAc3LfeFilter ::
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

pattern Ac3LfeFilter_DISABLED :: Ac3LfeFilter
pattern Ac3LfeFilter_DISABLED = Ac3LfeFilter' "DISABLED"

pattern Ac3LfeFilter_ENABLED :: Ac3LfeFilter
pattern Ac3LfeFilter_ENABLED = Ac3LfeFilter' "ENABLED"

{-# COMPLETE
  Ac3LfeFilter_DISABLED,
  Ac3LfeFilter_ENABLED,
  Ac3LfeFilter'
  #-}
