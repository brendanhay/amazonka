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
-- Module      : Network.AWS.MediaConvert.Types.H264EntropyEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264EntropyEncoding
  ( H264EntropyEncoding
      ( ..,
        H264EntropyEncoding_CABAC,
        H264EntropyEncoding_CAVLC
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
newtype H264EntropyEncoding = H264EntropyEncoding'
  { fromH264EntropyEncoding ::
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

pattern H264EntropyEncoding_CABAC :: H264EntropyEncoding
pattern H264EntropyEncoding_CABAC = H264EntropyEncoding' "CABAC"

pattern H264EntropyEncoding_CAVLC :: H264EntropyEncoding
pattern H264EntropyEncoding_CAVLC = H264EntropyEncoding' "CAVLC"

{-# COMPLETE
  H264EntropyEncoding_CABAC,
  H264EntropyEncoding_CAVLC,
  H264EntropyEncoding'
  #-}
