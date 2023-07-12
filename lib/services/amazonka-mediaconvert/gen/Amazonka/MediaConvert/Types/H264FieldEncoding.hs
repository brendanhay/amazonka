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
-- Module      : Amazonka.MediaConvert.Types.H264FieldEncoding
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264FieldEncoding
  ( H264FieldEncoding
      ( ..,
        H264FieldEncoding_FORCE_FIELD,
        H264FieldEncoding_MBAFF,
        H264FieldEncoding_PAFF
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The video encoding method for your MPEG-4 AVC output. Keep the default
-- value, PAFF, to have MediaConvert use PAFF encoding for interlaced
-- outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and
-- create separate interlaced fields. Choose MBAFF to disable PAFF and have
-- MediaConvert use MBAFF encoding for interlaced outputs.
newtype H264FieldEncoding = H264FieldEncoding'
  { fromH264FieldEncoding ::
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

pattern H264FieldEncoding_FORCE_FIELD :: H264FieldEncoding
pattern H264FieldEncoding_FORCE_FIELD = H264FieldEncoding' "FORCE_FIELD"

pattern H264FieldEncoding_MBAFF :: H264FieldEncoding
pattern H264FieldEncoding_MBAFF = H264FieldEncoding' "MBAFF"

pattern H264FieldEncoding_PAFF :: H264FieldEncoding
pattern H264FieldEncoding_PAFF = H264FieldEncoding' "PAFF"

{-# COMPLETE
  H264FieldEncoding_FORCE_FIELD,
  H264FieldEncoding_MBAFF,
  H264FieldEncoding_PAFF,
  H264FieldEncoding'
  #-}
