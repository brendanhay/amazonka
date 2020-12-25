{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.H264FieldEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.H264FieldEncoding
  ( H264FieldEncoding
      ( H264FieldEncoding',
        H264FieldEncodingPaff,
        H264FieldEncodingForceField,
        fromH264FieldEncoding
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Keep the default value, PAFF, to have MediaConvert use PAFF encoding for interlaced outputs. Choose Force field (FORCE_FIELD) to disable PAFF encoding and create separate interlaced fields.
newtype H264FieldEncoding = H264FieldEncoding'
  { fromH264FieldEncoding ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern H264FieldEncodingPaff :: H264FieldEncoding
pattern H264FieldEncodingPaff = H264FieldEncoding' "PAFF"

pattern H264FieldEncodingForceField :: H264FieldEncoding
pattern H264FieldEncodingForceField = H264FieldEncoding' "FORCE_FIELD"

{-# COMPLETE
  H264FieldEncodingPaff,
  H264FieldEncodingForceField,
  H264FieldEncoding'
  #-}
