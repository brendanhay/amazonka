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
-- Module      : Network.AWS.MediaConvert.Types.H265Tiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H265Tiles
  ( H265Tiles
      ( ..,
        H265Tiles_DISABLED,
        H265Tiles_ENABLED
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Enable use of tiles, allowing horizontal as well as vertical subdivision
-- of the encoded pictures.
newtype H265Tiles = H265Tiles'
  { fromH265Tiles ::
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

pattern H265Tiles_DISABLED :: H265Tiles
pattern H265Tiles_DISABLED = H265Tiles' "DISABLED"

pattern H265Tiles_ENABLED :: H265Tiles
pattern H265Tiles_ENABLED = H265Tiles' "ENABLED"

{-# COMPLETE
  H265Tiles_DISABLED,
  H265Tiles_ENABLED,
  H265Tiles'
  #-}
