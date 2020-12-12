{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.M2tsKlv
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.M2tsKlv
  ( M2tsKlv
      ( M2tsKlv',
        MKNone,
        MKPassthrough
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | M2ts Klv
newtype M2tsKlv = M2tsKlv' Lude.Text
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

pattern MKNone :: M2tsKlv
pattern MKNone = M2tsKlv' "NONE"

pattern MKPassthrough :: M2tsKlv
pattern MKPassthrough = M2tsKlv' "PASSTHROUGH"

{-# COMPLETE
  MKNone,
  MKPassthrough,
  M2tsKlv'
  #-}
