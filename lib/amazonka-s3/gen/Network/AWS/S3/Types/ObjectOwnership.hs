{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectOwnership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectOwnership
  ( ObjectOwnership
      ( ObjectOwnership',
        ObjectOwnershipBucketOwnerPreferred,
        ObjectOwnershipObjectWriter,
        fromObjectOwnership
      ),
  )
where

import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types

-- | The container element for object ownership for a bucket's ownership controls.
--
-- BucketOwnerPreferred - Objects uploaded to the bucket change ownership to the bucket owner if the objects are uploaded with the @bucket-owner-full-control@ canned ACL.
-- ObjectWriter - The uploading account will own the object if the object is uploaded with the @bucket-owner-full-control@ canned ACL.
newtype ObjectOwnership = ObjectOwnership'
  { fromObjectOwnership ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ObjectOwnershipBucketOwnerPreferred :: ObjectOwnership
pattern ObjectOwnershipBucketOwnerPreferred = ObjectOwnership' "BucketOwnerPreferred"

pattern ObjectOwnershipObjectWriter :: ObjectOwnership
pattern ObjectOwnershipObjectWriter = ObjectOwnership' "ObjectWriter"

{-# COMPLETE
  ObjectOwnershipBucketOwnerPreferred,
  ObjectOwnershipObjectWriter,
  ObjectOwnership'
  #-}
