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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ObjectCannedACL = ObjectCannedACL'
  { fromObjectCannedACL ::
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
