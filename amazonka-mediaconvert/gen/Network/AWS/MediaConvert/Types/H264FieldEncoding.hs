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

import qualified Network.AWS.Prelude as Prelude

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for
-- interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF
-- encoding and create separate interlaced fields.
newtype H264FieldEncoding = H264FieldEncoding'
  { fromH264FieldEncoding ::
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

pattern H264FieldEncoding_FORCE_FIELD :: H264FieldEncoding
pattern H264FieldEncoding_FORCE_FIELD = H264FieldEncoding' "FORCE_FIELD"

pattern H264FieldEncoding_PAFF :: H264FieldEncoding
pattern H264FieldEncoding_PAFF = H264FieldEncoding' "PAFF"

{-# COMPLETE
  H264FieldEncoding_FORCE_FIELD,
  H264FieldEncoding_PAFF,
  H264FieldEncoding'
  #-}
