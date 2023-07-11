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
-- Module      : Amazonka.StorageGateway.Types.ObjectACL
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.ObjectACL
  ( ObjectACL
      ( ..,
        ObjectACL_Authenticated_read,
        ObjectACL_Aws_exec_read,
        ObjectACL_Bucket_owner_full_control,
        ObjectACL_Bucket_owner_read,
        ObjectACL_Private,
        ObjectACL_Public_read,
        ObjectACL_Public_read_write
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that an S3 File Gateway puts objects into. The default
-- value is @private@.
newtype ObjectACL = ObjectACL'
  { fromObjectACL ::
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

pattern ObjectACL_Authenticated_read :: ObjectACL
pattern ObjectACL_Authenticated_read = ObjectACL' "authenticated-read"

pattern ObjectACL_Aws_exec_read :: ObjectACL
pattern ObjectACL_Aws_exec_read = ObjectACL' "aws-exec-read"

pattern ObjectACL_Bucket_owner_full_control :: ObjectACL
pattern ObjectACL_Bucket_owner_full_control = ObjectACL' "bucket-owner-full-control"

pattern ObjectACL_Bucket_owner_read :: ObjectACL
pattern ObjectACL_Bucket_owner_read = ObjectACL' "bucket-owner-read"

pattern ObjectACL_Private :: ObjectACL
pattern ObjectACL_Private = ObjectACL' "private"

pattern ObjectACL_Public_read :: ObjectACL
pattern ObjectACL_Public_read = ObjectACL' "public-read"

pattern ObjectACL_Public_read_write :: ObjectACL
pattern ObjectACL_Public_read_write = ObjectACL' "public-read-write"

{-# COMPLETE
  ObjectACL_Authenticated_read,
  ObjectACL_Aws_exec_read,
  ObjectACL_Bucket_owner_full_control,
  ObjectACL_Bucket_owner_read,
  ObjectACL_Private,
  ObjectACL_Public_read,
  ObjectACL_Public_read_write,
  ObjectACL'
  #-}
