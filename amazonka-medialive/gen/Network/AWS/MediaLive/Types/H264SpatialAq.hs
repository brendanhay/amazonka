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
-- Module      : Network.AWS.MediaLive.Types.H264SpatialAq
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H264SpatialAq
  ( H264SpatialAq
      ( ..,
        H264SpatialAq_DISABLED,
        H264SpatialAq_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

-- | H264 Spatial Aq
newtype H264SpatialAq = H264SpatialAq'
  { fromH264SpatialAq ::
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

pattern H264SpatialAq_DISABLED :: H264SpatialAq
pattern H264SpatialAq_DISABLED = H264SpatialAq' "DISABLED"

pattern H264SpatialAq_ENABLED :: H264SpatialAq
pattern H264SpatialAq_ENABLED = H264SpatialAq' "ENABLED"

{-# COMPLETE
  H264SpatialAq_DISABLED,
  H264SpatialAq_ENABLED,
  H264SpatialAq'
  #-}
