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
-- Module      : Amazonka.S3.Types.ObjectCannedACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectCannedACL
  ( ObjectCannedACL
      ( ..,
        ObjectCannedACL_Authenticated_read,
        ObjectCannedACL_Aws_exec_read,
        ObjectCannedACL_Bucket_owner_full_control,
        ObjectCannedACL_Bucket_owner_read,
        ObjectCannedACL_Private,
        ObjectCannedACL_Public_read,
        ObjectCannedACL_Public_read_write
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ObjectCannedACL = ObjectCannedACL'
  { fromObjectCannedACL ::
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

pattern ObjectCannedACL_Authenticated_read :: ObjectCannedACL
pattern ObjectCannedACL_Authenticated_read = ObjectCannedACL' "authenticated-read"

pattern ObjectCannedACL_Aws_exec_read :: ObjectCannedACL
pattern ObjectCannedACL_Aws_exec_read = ObjectCannedACL' "aws-exec-read"

pattern ObjectCannedACL_Bucket_owner_full_control :: ObjectCannedACL
pattern ObjectCannedACL_Bucket_owner_full_control = ObjectCannedACL' "bucket-owner-full-control"

pattern ObjectCannedACL_Bucket_owner_read :: ObjectCannedACL
pattern ObjectCannedACL_Bucket_owner_read = ObjectCannedACL' "bucket-owner-read"

pattern ObjectCannedACL_Private :: ObjectCannedACL
pattern ObjectCannedACL_Private = ObjectCannedACL' "private"

pattern ObjectCannedACL_Public_read :: ObjectCannedACL
pattern ObjectCannedACL_Public_read = ObjectCannedACL' "public-read"

pattern ObjectCannedACL_Public_read_write :: ObjectCannedACL
pattern ObjectCannedACL_Public_read_write = ObjectCannedACL' "public-read-write"

{-# COMPLETE
  ObjectCannedACL_Authenticated_read,
  ObjectCannedACL_Aws_exec_read,
  ObjectCannedACL_Bucket_owner_full_control,
  ObjectCannedACL_Bucket_owner_read,
  ObjectCannedACL_Private,
  ObjectCannedACL_Public_read,
  ObjectCannedACL_Public_read_write,
  ObjectCannedACL'
  #-}
