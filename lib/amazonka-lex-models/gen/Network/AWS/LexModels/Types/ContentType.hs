-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.ContentType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.ContentType
  ( ContentType
      ( ContentType',
        CustomPayload,
        PlainText,
        Ssml
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ContentType = ContentType' Lude.Text
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

pattern CustomPayload :: ContentType
pattern CustomPayload = ContentType' "CustomPayload"

pattern PlainText :: ContentType
pattern PlainText = ContentType' "PlainText"

pattern Ssml :: ContentType
pattern Ssml = ContentType' "SSML"

{-# COMPLETE
  CustomPayload,
  PlainText,
  Ssml,
  ContentType'
  #-}
