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

import qualified Network.AWS.Core as Core
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
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern ObjectOwnership_BucketOwnerPreferred :: ObjectOwnership
pattern ObjectOwnership_BucketOwnerPreferred = ObjectOwnership' "BucketOwnerPreferred"

pattern ObjectOwnership_ObjectWriter :: ObjectOwnership
pattern ObjectOwnership_ObjectWriter = ObjectOwnership' "ObjectWriter"

{-# COMPLETE
  ObjectOwnership_BucketOwnerPreferred,
  ObjectOwnership_ObjectWriter,
  ObjectOwnership'
  #-}
