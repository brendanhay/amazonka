{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.MsSmoothManifestEncoding
  ( MsSmoothManifestEncoding
      ( MsSmoothManifestEncoding',
        UTF8,
        UTF16
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Use Manifest encoding (MsSmoothManifestEncoding) to specify the encoding format for the server and client manifest. Valid options are utf8 and utf16.
newtype MsSmoothManifestEncoding = MsSmoothManifestEncoding' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern UTF8 :: MsSmoothManifestEncoding
pattern UTF8 = MsSmoothManifestEncoding' "UTF8"

pattern UTF16 :: MsSmoothManifestEncoding
pattern UTF16 = MsSmoothManifestEncoding' "UTF16"

{-# COMPLETE
  UTF8,
  UTF16,
  MsSmoothManifestEncoding'
  #-}
