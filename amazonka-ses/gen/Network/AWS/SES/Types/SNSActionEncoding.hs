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
-- Module      : Network.AWS.SES.Types.SNSActionEncoding
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.SNSActionEncoding
  ( SNSActionEncoding
      ( ..,
        SNSActionEncoding_Base64,
        SNSActionEncoding_UTF_8
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype SNSActionEncoding = SNSActionEncoding'
  { fromSNSActionEncoding ::
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

pattern SNSActionEncoding_Base64 :: SNSActionEncoding
pattern SNSActionEncoding_Base64 = SNSActionEncoding' "Base64"

pattern SNSActionEncoding_UTF_8 :: SNSActionEncoding
pattern SNSActionEncoding_UTF_8 = SNSActionEncoding' "UTF-8"

{-# COMPLETE
  SNSActionEncoding_Base64,
  SNSActionEncoding_UTF_8,
  SNSActionEncoding'
  #-}
