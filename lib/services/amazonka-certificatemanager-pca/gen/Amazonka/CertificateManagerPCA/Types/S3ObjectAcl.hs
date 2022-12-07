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
-- Module      : Amazonka.CertificateManagerPCA.Types.S3ObjectAcl
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.S3ObjectAcl
  ( S3ObjectAcl
      ( ..,
        S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL,
        S3ObjectAcl_PUBLIC_READ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype S3ObjectAcl = S3ObjectAcl'
  { fromS3ObjectAcl ::
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

pattern S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL :: S3ObjectAcl
pattern S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL = S3ObjectAcl' "BUCKET_OWNER_FULL_CONTROL"

pattern S3ObjectAcl_PUBLIC_READ :: S3ObjectAcl
pattern S3ObjectAcl_PUBLIC_READ = S3ObjectAcl' "PUBLIC_READ"

{-# COMPLETE
  S3ObjectAcl_BUCKET_OWNER_FULL_CONTROL,
  S3ObjectAcl_PUBLIC_READ,
  S3ObjectAcl'
  #-}
