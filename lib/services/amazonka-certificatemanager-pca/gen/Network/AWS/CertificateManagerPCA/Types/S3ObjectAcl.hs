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
-- Module      : Network.AWS.CertificateManagerPCA.Types.S3ObjectAcl
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.S3ObjectAcl
  ( S3ObjectAcl
      ( ..,
        S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL,
        S3ObjectAcl_PUBLIC_READ
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype S3ObjectAcl = S3ObjectAcl'
  { fromS3ObjectAcl ::
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

pattern S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL :: S3ObjectAcl
pattern S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL = S3ObjectAcl' "BUCKET_OWNER_FULL_CONTROL"

pattern S3ObjectAcl_PUBLIC_READ :: S3ObjectAcl
pattern S3ObjectAcl_PUBLIC_READ = S3ObjectAcl' "PUBLIC_READ"

{-# COMPLETE
  S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL,
  S3ObjectAcl_PUBLIC_READ,
  S3ObjectAcl'
  #-}
