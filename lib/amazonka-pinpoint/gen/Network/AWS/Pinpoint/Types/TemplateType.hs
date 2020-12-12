{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.TemplateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateType
  ( TemplateType
      ( TemplateType',
        TTEmail,
        TTPush,
        TTSms,
        TTVoice
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype TemplateType = TemplateType' Lude.Text
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

pattern TTEmail :: TemplateType
pattern TTEmail = TemplateType' "EMAIL"

pattern TTPush :: TemplateType
pattern TTPush = TemplateType' "PUSH"

pattern TTSms :: TemplateType
pattern TTSms = TemplateType' "SMS"

pattern TTVoice :: TemplateType
pattern TTVoice = TemplateType' "VOICE"

{-# COMPLETE
  TTEmail,
  TTPush,
  TTSms,
  TTVoice,
  TemplateType'
  #-}
