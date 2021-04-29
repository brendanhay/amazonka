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
-- Module      : Network.AWS.S3.Types.StorageClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.StorageClass
  ( StorageClass
      ( ..,
        StorageClass_DEEP_ARCHIVE,
        StorageClass_GLACIER,
        StorageClass_INTELLIGENT_TIERING,
        StorageClass_ONEZONE_IA,
        StorageClass_OUTPOSTS,
        StorageClass_REDUCED_REDUNDANCY,
        StorageClass_STANDARD,
        StorageClass_STANDARD_IA
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype StorageClass = StorageClass'
  { fromStorageClass ::
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

pattern StorageClass_DEEP_ARCHIVE :: StorageClass
pattern StorageClass_DEEP_ARCHIVE = StorageClass' "DEEP_ARCHIVE"

pattern StorageClass_GLACIER :: StorageClass
pattern StorageClass_GLACIER = StorageClass' "GLACIER"

pattern StorageClass_INTELLIGENT_TIERING :: StorageClass
pattern StorageClass_INTELLIGENT_TIERING = StorageClass' "INTELLIGENT_TIERING"

pattern StorageClass_ONEZONE_IA :: StorageClass
pattern StorageClass_ONEZONE_IA = StorageClass' "ONEZONE_IA"

pattern StorageClass_OUTPOSTS :: StorageClass
pattern StorageClass_OUTPOSTS = StorageClass' "OUTPOSTS"

pattern StorageClass_REDUCED_REDUNDANCY :: StorageClass
pattern StorageClass_REDUCED_REDUNDANCY = StorageClass' "REDUCED_REDUNDANCY"

pattern StorageClass_STANDARD :: StorageClass
pattern StorageClass_STANDARD = StorageClass' "STANDARD"

pattern StorageClass_STANDARD_IA :: StorageClass
pattern StorageClass_STANDARD_IA = StorageClass' "STANDARD_IA"

{-# COMPLETE
  StorageClass_DEEP_ARCHIVE,
  StorageClass_GLACIER,
  StorageClass_INTELLIGENT_TIERING,
  StorageClass_ONEZONE_IA,
  StorageClass_OUTPOSTS,
  StorageClass_REDUCED_REDUNDANCY,
  StorageClass_STANDARD,
  StorageClass_STANDARD_IA,
  StorageClass'
  #-}
