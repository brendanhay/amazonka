-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.H265AlternativeTransferFunction
  ( H265AlternativeTransferFunction
      ( H265AlternativeTransferFunction',
        Insert,
        Omit
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | H265 Alternative Transfer Function
newtype H265AlternativeTransferFunction = H265AlternativeTransferFunction' Lude.Text
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

pattern Insert :: H265AlternativeTransferFunction
pattern Insert = H265AlternativeTransferFunction' "INSERT"

pattern Omit :: H265AlternativeTransferFunction
pattern Omit = H265AlternativeTransferFunction' "OMIT"

{-# COMPLETE
  Insert,
  Omit,
  H265AlternativeTransferFunction'
  #-}
