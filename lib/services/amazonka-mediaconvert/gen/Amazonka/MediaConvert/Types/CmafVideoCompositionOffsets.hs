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
-- Module      : Amazonka.MediaConvert.Types.CmafVideoCompositionOffsets
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CmafVideoCompositionOffsets
  ( CmafVideoCompositionOffsets
      ( ..,
        CmafVideoCompositionOffsets_SIGNED,
        CmafVideoCompositionOffsets_UNSIGNED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the video sample composition time offset mode in the output fMP4
-- TRUN box. For wider player compatibility, set Video composition offsets
-- to Unsigned or leave blank. The earliest presentation time may be
-- greater than zero, and sample composition time offsets will increment
-- using unsigned integers. For strict fMP4 video and audio timing, set
-- Video composition offsets to Signed. The earliest presentation time will
-- be equal to zero, and sample composition time offsets will increment
-- using signed integers.
newtype CmafVideoCompositionOffsets = CmafVideoCompositionOffsets'
  { fromCmafVideoCompositionOffsets ::
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

pattern CmafVideoCompositionOffsets_SIGNED :: CmafVideoCompositionOffsets
pattern CmafVideoCompositionOffsets_SIGNED = CmafVideoCompositionOffsets' "SIGNED"

pattern CmafVideoCompositionOffsets_UNSIGNED :: CmafVideoCompositionOffsets
pattern CmafVideoCompositionOffsets_UNSIGNED = CmafVideoCompositionOffsets' "UNSIGNED"

{-# COMPLETE
  CmafVideoCompositionOffsets_SIGNED,
  CmafVideoCompositionOffsets_UNSIGNED,
  CmafVideoCompositionOffsets'
  #-}
