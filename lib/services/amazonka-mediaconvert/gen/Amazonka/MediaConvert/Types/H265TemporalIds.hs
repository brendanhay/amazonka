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
-- Module      : Amazonka.MediaConvert.Types.H265TemporalIds
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H265TemporalIds
  ( H265TemporalIds
      ( ..,
        H265TemporalIds_DISABLED,
        H265TemporalIds_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enables temporal layer identifiers in the encoded bitstream. Up to 3
-- layers are supported depending on GOP structure: I- and P-frames form
-- one layer, reference B-frames can form a second layer and non-reference
-- b-frames can form a third layer. Decoders can optionally decode only the
-- lower temporal layers to generate a lower frame rate output. For
-- example, given a bitstream with temporal IDs and with b-frames = 1 (i.e.
-- IbPbPb display order), a decoder could decode all the frames for full
-- frame rate output or only the I and P frames (lowest temporal layer) for
-- a half frame rate output.
newtype H265TemporalIds = H265TemporalIds'
  { fromH265TemporalIds ::
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

pattern H265TemporalIds_DISABLED :: H265TemporalIds
pattern H265TemporalIds_DISABLED = H265TemporalIds' "DISABLED"

pattern H265TemporalIds_ENABLED :: H265TemporalIds
pattern H265TemporalIds_ENABLED = H265TemporalIds' "ENABLED"

{-# COMPLETE
  H265TemporalIds_DISABLED,
  H265TemporalIds_ENABLED,
  H265TemporalIds'
  #-}
