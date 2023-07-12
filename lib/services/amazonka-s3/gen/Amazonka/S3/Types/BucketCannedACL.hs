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
-- Module      : Amazonka.S3.Types.BucketCannedACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.BucketCannedACL
  ( BucketCannedACL
      ( ..,
        BucketCannedACL_Authenticated_read,
        BucketCannedACL_Private,
        BucketCannedACL_Public_read,
        BucketCannedACL_Public_read_write
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype BucketCannedACL = BucketCannedACL'
  { fromBucketCannedACL ::
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

pattern BucketCannedACL_Authenticated_read :: BucketCannedACL
pattern BucketCannedACL_Authenticated_read = BucketCannedACL' "authenticated-read"

pattern BucketCannedACL_Private :: BucketCannedACL
pattern BucketCannedACL_Private = BucketCannedACL' "private"

pattern BucketCannedACL_Public_read :: BucketCannedACL
pattern BucketCannedACL_Public_read = BucketCannedACL' "public-read"

pattern BucketCannedACL_Public_read_write :: BucketCannedACL
pattern BucketCannedACL_Public_read_write = BucketCannedACL' "public-read-write"

{-# COMPLETE
  BucketCannedACL_Authenticated_read,
  BucketCannedACL_Private,
  BucketCannedACL_Public_read,
  BucketCannedACL_Public_read_write,
  BucketCannedACL'
  #-}
