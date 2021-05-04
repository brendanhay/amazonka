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

import qualified Network.AWS.Prelude as Prelude

-- | Entropy encoding mode. Use CABAC (must be in Main or High profile) or
-- CAVLC.
newtype H264EntropyEncoding = H264EntropyEncoding'
  { fromH264EntropyEncoding ::
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

pattern H264EntropyEncoding_CABAC :: H264EntropyEncoding
pattern H264EntropyEncoding_CABAC = H264EntropyEncoding' "CABAC"

pattern H264EntropyEncoding_CAVLC :: H264EntropyEncoding
pattern H264EntropyEncoding_CAVLC = H264EntropyEncoding' "CAVLC"

{-# COMPLETE
  H264EntropyEncoding_CABAC,
  H264EntropyEncoding_CAVLC,
  H264EntropyEncoding'
  #-}
