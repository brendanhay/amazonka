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

import qualified Network.AWS.Prelude as Prelude

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding
-- format for the server and client manifest. Valid options are utf8 and
-- utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding'
  { fromMsSmoothManifestEncoding ::
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

pattern MsSmoothManifestEncoding_UTF16 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncoding_UTF16 = MsSmoothManifestEncoding' "UTF16"

pattern MsSmoothManifestEncoding_UTF8 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncoding_UTF8 = MsSmoothManifestEncoding' "UTF8"

{-# COMPLETE
  MsSmoothManifestEncoding_UTF16,
  MsSmoothManifestEncoding_UTF8,
  MsSmoothManifestEncoding'
  #-}
