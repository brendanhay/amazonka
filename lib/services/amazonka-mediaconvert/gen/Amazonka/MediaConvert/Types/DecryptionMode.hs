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
-- Module      : Amazonka.MediaConvert.Types.DecryptionMode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.DecryptionMode
  ( DecryptionMode
      ( ..,
        DecryptionMode_AES_CBC,
        DecryptionMode_AES_CTR,
        DecryptionMode_AES_GCM
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specify the encryption mode that you used to encrypt your input files.
newtype DecryptionMode = DecryptionMode'
  { fromDecryptionMode ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
