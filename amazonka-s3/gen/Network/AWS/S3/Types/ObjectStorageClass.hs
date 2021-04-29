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
-- Module      : Network.AWS.S3.Types.ObjectStorageClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectStorageClass
  ( ObjectStorageClass
      ( ..,
        ObjectStorageClass_GLACIER,
        ObjectStorageClass_INTELLIGENT_TIERING,
        ObjectStorageClass_REDUCED_REDUNDANCY,
        ObjectStorageClass_STANDARD,
        ObjectStorageClass_STANDARD_IA
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype ObjectStorageClass = ObjectStorageClass'
  { fromObjectStorageClass ::
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

pattern ObjectStorageClass_GLACIER :: ObjectStorageClass
pattern ObjectStorageClass_GLACIER = ObjectStorageClass' "GLACIER"

pattern ObjectStorageClass_INTELLIGENT_TIERING :: ObjectStorageClass
pattern ObjectStorageClass_INTELLIGENT_TIERING = ObjectStorageClass' "INTELLIGENT_TIERING"

pattern ObjectStorageClass_REDUCED_REDUNDANCY :: ObjectStorageClass
pattern ObjectStorageClass_REDUCED_REDUNDANCY = ObjectStorageClass' "REDUCED_REDUNDANCY"

pattern ObjectStorageClass_STANDARD :: ObjectStorageClass
pattern ObjectStorageClass_STANDARD = ObjectStorageClass' "STANDARD"

pattern ObjectStorageClass_STANDARD_IA :: ObjectStorageClass
pattern ObjectStorageClass_STANDARD_IA = ObjectStorageClass' "STANDARD_IA"

{-# COMPLETE
  ObjectStorageClass_GLACIER,
  ObjectStorageClass_INTELLIGENT_TIERING,
  ObjectStorageClass_REDUCED_REDUNDANCY,
  ObjectStorageClass_STANDARD,
  ObjectStorageClass_STANDARD_IA,
  ObjectStorageClass'
  #-}
