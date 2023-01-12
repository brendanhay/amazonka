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
-- Module      : Amazonka.S3.Types.ChecksumAlgorithm
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ChecksumAlgorithm
  ( ChecksumAlgorithm
      ( ..,
        ChecksumAlgorithm_CRC32,
        ChecksumAlgorithm_CRC32C,
        ChecksumAlgorithm_SHA1,
        ChecksumAlgorithm_SHA256
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ChecksumAlgorithm = ChecksumAlgorithm'
  { fromChecksumAlgorithm ::
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

pattern ChecksumAlgorithm_CRC32 :: ChecksumAlgorithm
pattern ChecksumAlgorithm_CRC32 = ChecksumAlgorithm' "CRC32"

pattern ChecksumAlgorithm_CRC32C :: ChecksumAlgorithm
pattern ChecksumAlgorithm_CRC32C = ChecksumAlgorithm' "CRC32C"

pattern ChecksumAlgorithm_SHA1 :: ChecksumAlgorithm
pattern ChecksumAlgorithm_SHA1 = ChecksumAlgorithm' "SHA1"

pattern ChecksumAlgorithm_SHA256 :: ChecksumAlgorithm
pattern ChecksumAlgorithm_SHA256 = ChecksumAlgorithm' "SHA256"

{-# COMPLETE
  ChecksumAlgorithm_CRC32,
  ChecksumAlgorithm_CRC32C,
  ChecksumAlgorithm_SHA1,
  ChecksumAlgorithm_SHA256,
  ChecksumAlgorithm'
  #-}
