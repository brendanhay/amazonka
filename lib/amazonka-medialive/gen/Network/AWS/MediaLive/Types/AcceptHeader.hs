-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.AcceptHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.AcceptHeader
  ( AcceptHeader
      ( AcceptHeader',
        AHImageJpeg
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The HTTP Accept header. Indicates the requested type fothe thumbnail.
newtype AcceptHeader = AcceptHeader' Lude.Text
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

pattern AHImageJpeg :: AcceptHeader
pattern AHImageJpeg = AcceptHeader' "image/jpeg"

{-# COMPLETE
  AHImageJpeg,
  AcceptHeader'
  #-}
