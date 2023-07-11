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
-- Module      : Amazonka.MediaLive.Types.HlsTimedMetadataId3Frame
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.HlsTimedMetadataId3Frame
  ( HlsTimedMetadataId3Frame
      ( ..,
        HlsTimedMetadataId3Frame_NONE,
        HlsTimedMetadataId3Frame_PRIV,
        HlsTimedMetadataId3Frame_TDRL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Hls Timed Metadata Id3 Frame
newtype HlsTimedMetadataId3Frame = HlsTimedMetadataId3Frame'
  { fromHlsTimedMetadataId3Frame ::
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

pattern HlsTimedMetadataId3Frame_NONE :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3Frame_NONE = HlsTimedMetadataId3Frame' "NONE"

pattern HlsTimedMetadataId3Frame_PRIV :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3Frame_PRIV = HlsTimedMetadataId3Frame' "PRIV"

pattern HlsTimedMetadataId3Frame_TDRL :: HlsTimedMetadataId3Frame
pattern HlsTimedMetadataId3Frame_TDRL = HlsTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  HlsTimedMetadataId3Frame_NONE,
  HlsTimedMetadataId3Frame_PRIV,
  HlsTimedMetadataId3Frame_TDRL,
  HlsTimedMetadataId3Frame'
  #-}
