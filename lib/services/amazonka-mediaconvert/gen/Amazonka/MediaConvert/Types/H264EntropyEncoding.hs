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
-- Module      : Amazonka.MediaConvert.Types.H264EntropyEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264EntropyEncoding
  ( H264EntropyEncoding
      ( ..,
        H264EntropyEncoding_CABAC,
        H264EntropyEncoding_CAVLC
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
newtype H264EntropyEncoding = H264EntropyEncoding'
  { fromH264EntropyEncoding ::
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

pattern H264EntropyEncoding_CABAC :: H264EntropyEncoding
pattern H264EntropyEncoding_CABAC = H264EntropyEncoding' "CABAC"

pattern H264EntropyEncoding_CAVLC :: H264EntropyEncoding
pattern H264EntropyEncoding_CAVLC = H264EntropyEncoding' "CAVLC"

{-# COMPLETE
  H264EntropyEncoding_CABAC,
  H264EntropyEncoding_CAVLC,
  H264EntropyEncoding'
  #-}
