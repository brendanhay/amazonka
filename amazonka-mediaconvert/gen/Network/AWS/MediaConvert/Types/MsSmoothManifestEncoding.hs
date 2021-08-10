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
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
  ( MsSmoothManifestEncoding
      ( ..,
        MsSmoothManifestEncoding_UTF16,
        MsSmoothManifestEncoding_UTF8
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding'
  { fromMsSmoothManifestEncoding ::
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

pattern MsSmoothManifestEncoding_UTF16 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncoding_UTF16 = MsSmoothManifestEncoding' "UTF16"

pattern MsSmoothManifestEncoding_UTF8 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncoding_UTF8 = MsSmoothManifestEncoding' "UTF8"

{-# COMPLETE
  MsSmoothManifestEncoding_UTF16,
  MsSmoothManifestEncoding_UTF8,
  MsSmoothManifestEncoding'
  #-}
