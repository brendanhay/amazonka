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
-- Module      : Network.AWS.MediaLive.Types.Mp2CodingMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.Mp2CodingMode
  ( Mp2CodingMode
      ( ..,
        Mp2CodingMode_CODING_MODE_1_0,
        Mp2CodingMode_CODING_MODE_2_0
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Mp2 Coding Mode
newtype Mp2CodingMode = Mp2CodingMode'
  { fromMp2CodingMode ::
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

pattern Mp2CodingMode_CODING_MODE_1_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_1_0 = Mp2CodingMode' "CODING_MODE_1_0"

pattern Mp2CodingMode_CODING_MODE_2_0 :: Mp2CodingMode
pattern Mp2CodingMode_CODING_MODE_2_0 = Mp2CodingMode' "CODING_MODE_2_0"

{-# COMPLETE
  Mp2CodingMode_CODING_MODE_1_0,
  Mp2CodingMode_CODING_MODE_2_0,
  Mp2CodingMode'
  #-}
