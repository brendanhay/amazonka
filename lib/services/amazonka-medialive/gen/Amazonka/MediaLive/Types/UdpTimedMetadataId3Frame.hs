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
-- Module      : Amazonka.MediaLive.Types.UdpTimedMetadataId3Frame
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.UdpTimedMetadataId3Frame
  ( UdpTimedMetadataId3Frame
      ( ..,
        UdpTimedMetadataId3Frame_NONE,
        UdpTimedMetadataId3Frame_PRIV,
        UdpTimedMetadataId3Frame_TDRL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Udp Timed Metadata Id3 Frame
newtype UdpTimedMetadataId3Frame = UdpTimedMetadataId3Frame'
  { fromUdpTimedMetadataId3Frame ::
      Core.Text
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

pattern UdpTimedMetadataId3Frame_NONE :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3Frame_NONE = UdpTimedMetadataId3Frame' "NONE"

pattern UdpTimedMetadataId3Frame_PRIV :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3Frame_PRIV = UdpTimedMetadataId3Frame' "PRIV"

pattern UdpTimedMetadataId3Frame_TDRL :: UdpTimedMetadataId3Frame
pattern UdpTimedMetadataId3Frame_TDRL = UdpTimedMetadataId3Frame' "TDRL"

{-# COMPLETE
  UdpTimedMetadataId3Frame_NONE,
  UdpTimedMetadataId3Frame_PRIV,
  UdpTimedMetadataId3Frame_TDRL,
  UdpTimedMetadataId3Frame'
  #-}
