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
-- Module      : Amazonka.CodeBuild.Types.BucketOwnerAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.BucketOwnerAccess
  ( BucketOwnerAccess
      ( ..,
        BucketOwnerAccess_FULL,
        BucketOwnerAccess_NONE,
        BucketOwnerAccess_READ_ONLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the bucket owner\'s access for objects that another account
-- uploads to their Amazon S3 bucket. By default, only the account that
-- uploads the objects to the bucket has access to these objects. This
-- property allows you to give the bucket owner access to these objects.
--
-- To use this property, your CodeBuild service role must have the
-- @s3:PutBucketAcl@ permission. This permission allows CodeBuild to modify
-- the access control list for the bucket.
--
-- This property can be one of the following values:
--
-- [NONE]
--     The bucket owner does not have access to the objects. This is the
--     default.
--
-- [READ_ONLY]
--     The bucket owner has read-only access to the objects. The uploading
--     account retains ownership of the objects.
--
-- [FULL]
--     The bucket owner has full access to the objects. Object ownership is
--     determined by the following criteria:
--
--     -   If the bucket is configured with the __Bucket owner preferred__
--         setting, the bucket owner owns the objects. The uploading
--         account will have object access as specified by the bucket\'s
--         policy.
--
--     -   Otherwise, the uploading account retains ownership of the
--         objects.
--
--     For more information about Amazon S3 object ownership, see
--     <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Controlling ownership of uploaded objects using S3 Object Ownership>
--     in the /Amazon Simple Storage Service User Guide/.
newtype BucketOwnerAccess = BucketOwnerAccess'
  { fromBucketOwnerAccess ::
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

pattern BucketOwnerAccess_FULL :: BucketOwnerAccess
pattern BucketOwnerAccess_FULL = BucketOwnerAccess' "FULL"

pattern BucketOwnerAccess_NONE :: BucketOwnerAccess
pattern BucketOwnerAccess_NONE = BucketOwnerAccess' "NONE"

pattern BucketOwnerAccess_READ_ONLY :: BucketOwnerAccess
pattern BucketOwnerAccess_READ_ONLY = BucketOwnerAccess' "READ_ONLY"

{-# COMPLETE
  BucketOwnerAccess_FULL,
  BucketOwnerAccess_NONE,
  BucketOwnerAccess_READ_ONLY,
  BucketOwnerAccess'
  #-}
