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
-- Module      : Network.AWS.S3.Types.ObjectOwnership
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectOwnership
  ( ObjectOwnership
      ( ..,
        ObjectOwnership_BucketOwnerPreferred,
        ObjectOwnership_ObjectWriter
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | The container element for object ownership for a bucket\'s ownership
-- controls.
--
-- BucketOwnerPreferred - Objects uploaded to the bucket change ownership
-- to the bucket owner if the objects are uploaded with the
-- @bucket-owner-full-control@ canned ACL.
--
-- ObjectWriter - The uploading account will own the object if the object
-- is uploaded with the @bucket-owner-full-control@ canned ACL.
newtype ObjectOwnership = ObjectOwnership'
  { fromObjectOwnership ::
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

pattern ObjectOwnership_BucketOwnerPreferred :: ObjectOwnership
pattern ObjectOwnership_BucketOwnerPreferred = ObjectOwnership' "BucketOwnerPreferred"

pattern ObjectOwnership_ObjectWriter :: ObjectOwnership
pattern ObjectOwnership_ObjectWriter = ObjectOwnership' "ObjectWriter"

{-# COMPLETE
  ObjectOwnership_BucketOwnerPreferred,
  ObjectOwnership_ObjectWriter,
  ObjectOwnership'
  #-}
