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
-- Module      : Amazonka.MediaLive.Types.S3CannedAcl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.S3CannedAcl
  ( S3CannedAcl
      ( ..,
        S3CannedAcl_AUTHENTICATED_READ,
        S3CannedAcl_BUCKET_OWNER_FULL_CONTROL,
        S3CannedAcl_BUCKET_OWNER_READ,
        S3CannedAcl_PUBLIC_READ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | S3 Canned Acl
newtype S3CannedAcl = S3CannedAcl'
  { fromS3CannedAcl ::
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

pattern S3CannedAcl_AUTHENTICATED_READ :: S3CannedAcl
pattern S3CannedAcl_AUTHENTICATED_READ = S3CannedAcl' "AUTHENTICATED_READ"

pattern S3CannedAcl_BUCKET_OWNER_FULL_CONTROL :: S3CannedAcl
pattern S3CannedAcl_BUCKET_OWNER_FULL_CONTROL = S3CannedAcl' "BUCKET_OWNER_FULL_CONTROL"

pattern S3CannedAcl_BUCKET_OWNER_READ :: S3CannedAcl
pattern S3CannedAcl_BUCKET_OWNER_READ = S3CannedAcl' "BUCKET_OWNER_READ"

pattern S3CannedAcl_PUBLIC_READ :: S3CannedAcl
pattern S3CannedAcl_PUBLIC_READ = S3CannedAcl' "PUBLIC_READ"

{-# COMPLETE
  S3CannedAcl_AUTHENTICATED_READ,
  S3CannedAcl_BUCKET_OWNER_FULL_CONTROL,
  S3CannedAcl_BUCKET_OWNER_READ,
  S3CannedAcl_PUBLIC_READ,
  S3CannedAcl'
  #-}
