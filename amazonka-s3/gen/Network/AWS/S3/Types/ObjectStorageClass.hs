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

import qualified Network.AWS.Core as Core
import Network.AWS.S3.Internal

newtype ObjectStorageClass = ObjectStorageClass'
  { fromObjectStorageClass ::
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
