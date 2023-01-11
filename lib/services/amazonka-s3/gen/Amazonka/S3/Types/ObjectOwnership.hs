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
-- Module      : Amazonka.S3.Types.ObjectOwnership
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectOwnership
  ( ObjectOwnership
      ( ..,
        ObjectOwnership_BucketOwnerEnforced,
        ObjectOwnership_BucketOwnerPreferred,
        ObjectOwnership_ObjectWriter
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | The container element for object ownership for a bucket\'s ownership
-- controls.
--
-- BucketOwnerPreferred - Objects uploaded to the bucket change ownership
-- to the bucket owner if the objects are uploaded with the
-- @bucket-owner-full-control@ canned ACL.
--
-- ObjectWriter - The uploading account will own the object if the object
-- is uploaded with the @bucket-owner-full-control@ canned ACL.
--
-- BucketOwnerEnforced - Access control lists (ACLs) are disabled and no
-- longer affect permissions. The bucket owner automatically owns and has
-- full control over every object in the bucket. The bucket only accepts
-- PUT requests that don\'t specify an ACL or bucket owner full control
-- ACLs, such as the @bucket-owner-full-control@ canned ACL or an
-- equivalent form of this ACL expressed in the XML format.
newtype ObjectOwnership = ObjectOwnership'
  { fromObjectOwnership ::
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

pattern ObjectOwnership_BucketOwnerEnforced :: ObjectOwnership
pattern ObjectOwnership_BucketOwnerEnforced = ObjectOwnership' "BucketOwnerEnforced"

pattern ObjectOwnership_BucketOwnerPreferred :: ObjectOwnership
pattern ObjectOwnership_BucketOwnerPreferred = ObjectOwnership' "BucketOwnerPreferred"

pattern ObjectOwnership_ObjectWriter :: ObjectOwnership
pattern ObjectOwnership_ObjectWriter = ObjectOwnership' "ObjectWriter"

{-# COMPLETE
  ObjectOwnership_BucketOwnerEnforced,
  ObjectOwnership_BucketOwnerPreferred,
  ObjectOwnership_ObjectWriter,
  ObjectOwnership'
  #-}
