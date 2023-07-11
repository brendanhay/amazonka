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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HashAlgorithm = HashAlgorithm'
  { fromHashAlgorithm ::
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
