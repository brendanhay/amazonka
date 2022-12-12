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
-- Module      : Amazonka.MediaConvert.Types.CmafMpdManifestBandwidthType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafMpdManifestBandwidthType
  ( CmafMpdManifestBandwidthType
      ( ..,
        CmafMpdManifestBandwidthType_AVERAGE,
        CmafMpdManifestBandwidthType_MAX
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify how the value for bandwidth is determined for each video
-- Representation in your output MPD manifest. We recommend that you choose
-- a MPD manifest bandwidth type that is compatible with your downstream
-- player configuration. Max: Use the same value that you specify for Max
-- bitrate in the video output, in bits per second. Average: Use the
-- calculated average bitrate of the encoded video output, in bits per
-- second.
newtype CmafMpdManifestBandwidthType = CmafMpdManifestBandwidthType'
  { fromCmafMpdManifestBandwidthType ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern CmafMpdManifestBandwidthType_AVERAGE :: CmafMpdManifestBandwidthType
pattern CmafMpdManifestBandwidthType_AVERAGE = CmafMpdManifestBandwidthType' "AVERAGE"

pattern CmafMpdManifestBandwidthType_MAX :: CmafMpdManifestBandwidthType
pattern CmafMpdManifestBandwidthType_MAX = CmafMpdManifestBandwidthType' "MAX"

{-# COMPLETE
  CmafMpdManifestBandwidthType_AVERAGE,
  CmafMpdManifestBandwidthType_MAX,
  CmafMpdManifestBandwidthType'
  #-}
