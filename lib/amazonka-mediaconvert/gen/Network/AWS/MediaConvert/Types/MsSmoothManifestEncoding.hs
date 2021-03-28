{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
  ( MsSmoothManifestEncoding
    ( MsSmoothManifestEncoding'
    , MsSmoothManifestEncodingUTF8
    , MsSmoothManifestEncodingUTF16
    , fromMsSmoothManifestEncoding
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding'{fromMsSmoothManifestEncoding
                                                             :: Core.Text}
                                     deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                     Core.Generic)
                                     deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                       Core.ToJSONKey, Core.FromJSONKey,
                                                       Core.ToJSON, Core.FromJSON, Core.ToXML,
                                                       Core.FromXML, Core.ToText, Core.FromText,
                                                       Core.ToByteString, Core.ToQuery,
                                                       Core.ToHeader)

pattern MsSmoothManifestEncodingUTF8 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncodingUTF8 = MsSmoothManifestEncoding' "UTF8"

pattern MsSmoothManifestEncodingUTF16 :: MsSmoothManifestEncoding
pattern MsSmoothManifestEncodingUTF16 = MsSmoothManifestEncoding' "UTF16"

{-# COMPLETE 
  MsSmoothManifestEncodingUTF8,

  MsSmoothManifestEncodingUTF16,
  MsSmoothManifestEncoding'
  #-}
