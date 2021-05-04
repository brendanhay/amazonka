{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.ObjectACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.ObjectACL
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

import qualified Network.AWS.Prelude as Prelude

-- | A value that sets the access control list (ACL) permission for objects
-- in the S3 bucket that a file gateway puts objects into. The default
-- value is @private@.
newtype ObjectACL = ObjectACL'
  { fromObjectACL ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
