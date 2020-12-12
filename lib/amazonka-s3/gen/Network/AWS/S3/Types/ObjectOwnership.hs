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
        BucketOwnerPreferred,
        ObjectWriter
      ),
  )
where

import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal

-- | The container element for object ownership for a bucket's ownership controls.
--
-- BucketOwnerPreferred - Objects uploaded to the bucket change ownership to the bucket owner if the objects are uploaded with the @bucket-owner-full-control@ canned ACL.
-- ObjectWriter - The uploading account will own the object if the object is uploaded with the @bucket-owner-full-control@ canned ACL.
newtype ObjectOwnership = ObjectOwnership' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern BucketOwnerPreferred :: ObjectOwnership
pattern BucketOwnerPreferred = ObjectOwnership' "BucketOwnerPreferred"

pattern ObjectWriter :: ObjectOwnership
pattern ObjectWriter = ObjectOwnership' "ObjectWriter"

{-# COMPLETE
  BucketOwnerPreferred,
  ObjectWriter,
  ObjectOwnership'
  #-}
