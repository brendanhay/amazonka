{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexRuntime.Types.MessageFormatType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexRuntime.Types.MessageFormatType
  ( MessageFormatType
      ( MessageFormatType',
        PlainText,
        CustomPayload,
        Ssml,
        Composite
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MessageFormatType = MessageFormatType' Lude.Text
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

pattern PlainText :: MessageFormatType
pattern PlainText = MessageFormatType' "PlainText"

pattern CustomPayload :: MessageFormatType
pattern CustomPayload = MessageFormatType' "CustomPayload"

pattern Ssml :: MessageFormatType
pattern Ssml = MessageFormatType' "SSML"

pattern Composite :: MessageFormatType
pattern Composite = MessageFormatType' "Composite"

{-# COMPLETE
  PlainText,
  CustomPayload,
  Ssml,
  Composite,
  MessageFormatType'
  #-}
