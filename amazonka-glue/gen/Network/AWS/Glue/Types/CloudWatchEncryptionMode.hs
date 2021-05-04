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
-- Module      : Network.AWS.Glue.Types.CloudWatchEncryptionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CloudWatchEncryptionMode
  ( CloudWatchEncryptionMode
      ( ..,
        CloudWatchEncryptionMode_DISABLED,
        CloudWatchEncryptionMode_SSE_KMS
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype CloudWatchEncryptionMode = CloudWatchEncryptionMode'
  { fromCloudWatchEncryptionMode ::
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

pattern CloudWatchEncryptionMode_DISABLED :: CloudWatchEncryptionMode
pattern CloudWatchEncryptionMode_DISABLED = CloudWatchEncryptionMode' "DISABLED"

pattern CloudWatchEncryptionMode_SSE_KMS :: CloudWatchEncryptionMode
pattern CloudWatchEncryptionMode_SSE_KMS = CloudWatchEncryptionMode' "SSE-KMS"

{-# COMPLETE
  CloudWatchEncryptionMode_DISABLED,
  CloudWatchEncryptionMode_SSE_KMS,
  CloudWatchEncryptionMode'
  #-}
