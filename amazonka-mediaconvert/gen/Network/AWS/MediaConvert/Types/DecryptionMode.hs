{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DecryptionMode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DecryptionMode
  ( DecryptionMode
      ( ..,
        DecryptionMode_AES_CBC,
        DecryptionMode_AES_CTR,
        DecryptionMode_AES_GCM
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | Specify the encryption mode that you used to encrypt your input files.
newtype DecryptionMode = DecryptionMode'
  { fromDecryptionMode ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
    )

pattern DecryptionMode_AES_CBC :: DecryptionMode
pattern DecryptionMode_AES_CBC = DecryptionMode' "AES_CBC"

pattern DecryptionMode_AES_CTR :: DecryptionMode
pattern DecryptionMode_AES_CTR = DecryptionMode' "AES_CTR"

pattern DecryptionMode_AES_GCM :: DecryptionMode
pattern DecryptionMode_AES_GCM = DecryptionMode' "AES_GCM"

{-# COMPLETE
  DecryptionMode_AES_CBC,
  DecryptionMode_AES_CTR,
  DecryptionMode_AES_GCM,
  DecryptionMode'
  #-}
