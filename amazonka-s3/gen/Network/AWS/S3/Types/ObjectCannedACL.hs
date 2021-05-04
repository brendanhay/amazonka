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
-- Module      : Network.AWS.S3.Types.ObjectCannedACL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectCannedACL
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype ObjectCannedACL = ObjectCannedACL'
  { fromObjectCannedACL ::
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
