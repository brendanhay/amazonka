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
-- Module      : Network.AWS.MediaConvert.Types.H264FieldEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FieldEncoding
  ( H264FieldEncoding
      ( ..,
        H264FieldEncoding_FORCE_FIELD,
        H264FieldEncoding_PAFF
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for
-- interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF
-- encoding and create separate interlaced fields.
newtype H264FieldEncoding = H264FieldEncoding'
  { fromH264FieldEncoding ::
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

pattern H264FieldEncoding_FORCE_FIELD :: H264FieldEncoding
pattern H264FieldEncoding_FORCE_FIELD = H264FieldEncoding' "FORCE_FIELD"

pattern H264FieldEncoding_PAFF :: H264FieldEncoding
pattern H264FieldEncoding_PAFF = H264FieldEncoding' "PAFF"

{-# COMPLETE
  H264FieldEncoding_FORCE_FIELD,
  H264FieldEncoding_PAFF,
  H264FieldEncoding'
  #-}
