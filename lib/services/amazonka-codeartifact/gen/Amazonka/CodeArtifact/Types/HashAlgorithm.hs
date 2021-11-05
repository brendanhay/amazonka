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
-- Module      : Amazonka.CodeArtifact.Types.HashAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.HashAlgorithm
  ( HashAlgorithm
      ( ..,
        HashAlgorithm_MD5,
        HashAlgorithm_SHA_1,
        HashAlgorithm_SHA_256,
        HashAlgorithm_SHA_512
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype HashAlgorithm = HashAlgorithm'
  { fromHashAlgorithm ::
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

pattern HashAlgorithm_MD5 :: HashAlgorithm
pattern HashAlgorithm_MD5 = HashAlgorithm' "MD5"

pattern HashAlgorithm_SHA_1 :: HashAlgorithm
pattern HashAlgorithm_SHA_1 = HashAlgorithm' "SHA-1"

pattern HashAlgorithm_SHA_256 :: HashAlgorithm
pattern HashAlgorithm_SHA_256 = HashAlgorithm' "SHA-256"

pattern HashAlgorithm_SHA_512 :: HashAlgorithm
pattern HashAlgorithm_SHA_512 = HashAlgorithm' "SHA-512"

{-# COMPLETE
  HashAlgorithm_MD5,
  HashAlgorithm_SHA_1,
  HashAlgorithm_SHA_256,
  HashAlgorithm_SHA_512,
  HashAlgorithm'
  #-}
