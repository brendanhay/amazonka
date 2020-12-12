{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.SNSActionEncoding
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSActionEncoding
  ( SNSActionEncoding
      ( SNSActionEncoding',
        BASE64,
        Utf8
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SNSActionEncoding = SNSActionEncoding' Lude.Text
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

pattern BASE64 :: SNSActionEncoding
pattern BASE64 = SNSActionEncoding' "Base64"

pattern Utf8 :: SNSActionEncoding
pattern Utf8 = SNSActionEncoding' "UTF-8"

{-# COMPLETE
  BASE64,
  Utf8,
  SNSActionEncoding'
  #-}
