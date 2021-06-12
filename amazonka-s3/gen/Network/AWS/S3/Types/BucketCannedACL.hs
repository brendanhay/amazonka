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
-- Module      : Network.AWS.S3.Types.BucketCannedACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.BucketCannedACL
  ( BucketCannedACL
      ( ..,
        BucketCannedACL_Authenticated_read,
        BucketCannedACL_Private,
        BucketCannedACL_Public_read,
        BucketCannedACL_Public_read_write
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype BucketCannedACL = BucketCannedACL'
  { fromBucketCannedACL ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
