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
-- Module      : Amazonka.MediaConvert.Types.S3ObjectCannedAcl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.S3ObjectCannedAcl
  ( S3ObjectCannedAcl
      ( ..,
        S3ObjectCannedAcl_AUTHENTICATED_READ,
        S3ObjectCannedAcl_BUCKET_OWNER_FULL_CONTROL,
        S3ObjectCannedAcl_BUCKET_OWNER_READ,
        S3ObjectCannedAcl_PUBLIC_READ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Choose an Amazon S3 canned ACL for MediaConvert to apply to this output.
newtype S3ObjectCannedAcl = S3ObjectCannedAcl'
  { fromS3ObjectCannedAcl ::
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

pattern S3ObjectCannedAcl_AUTHENTICATED_READ :: S3ObjectCannedAcl
pattern S3ObjectCannedAcl_AUTHENTICATED_READ = S3ObjectCannedAcl' "AUTHENTICATED_READ"

pattern S3ObjectCannedAcl_BUCKET_OWNER_FULL_CONTROL :: S3ObjectCannedAcl
pattern S3ObjectCannedAcl_BUCKET_OWNER_FULL_CONTROL = S3ObjectCannedAcl' "BUCKET_OWNER_FULL_CONTROL"

pattern S3ObjectCannedAcl_BUCKET_OWNER_READ :: S3ObjectCannedAcl
pattern S3ObjectCannedAcl_BUCKET_OWNER_READ = S3ObjectCannedAcl' "BUCKET_OWNER_READ"

pattern S3ObjectCannedAcl_PUBLIC_READ :: S3ObjectCannedAcl
pattern S3ObjectCannedAcl_PUBLIC_READ = S3ObjectCannedAcl' "PUBLIC_READ"

{-# COMPLETE
  S3ObjectCannedAcl_AUTHENTICATED_READ,
  S3ObjectCannedAcl_BUCKET_OWNER_FULL_CONTROL,
  S3ObjectCannedAcl_BUCKET_OWNER_READ,
  S3ObjectCannedAcl_PUBLIC_READ,
  S3ObjectCannedAcl'
  #-}
