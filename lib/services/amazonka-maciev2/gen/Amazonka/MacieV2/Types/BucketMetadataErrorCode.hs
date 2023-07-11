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
-- Module      : Amazonka.MacieV2.Types.BucketMetadataErrorCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketMetadataErrorCode
  ( BucketMetadataErrorCode
      ( ..,
        BucketMetadataErrorCode_ACCESS_DENIED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The error code for an error that prevented Amazon Macie from retrieving
-- and processing metadata from Amazon S3 for an S3 bucket and the
-- bucket\'s objects.
newtype BucketMetadataErrorCode = BucketMetadataErrorCode'
  { fromBucketMetadataErrorCode ::
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

pattern BucketMetadataErrorCode_ACCESS_DENIED :: BucketMetadataErrorCode
pattern BucketMetadataErrorCode_ACCESS_DENIED = BucketMetadataErrorCode' "ACCESS_DENIED"

{-# COMPLETE
  BucketMetadataErrorCode_ACCESS_DENIED,
  BucketMetadataErrorCode'
  #-}
