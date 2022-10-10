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
-- Module      : Amazonka.S3.Types.ObjectAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectAttributes
  ( ObjectAttributes
      ( ..,
        ObjectAttributes_Checksum,
        ObjectAttributes_ETag,
        ObjectAttributes_ObjectParts,
        ObjectAttributes_ObjectSize,
        ObjectAttributes_StorageClass
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ObjectAttributes = ObjectAttributes'
  { fromObjectAttributes ::
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

pattern ObjectAttributes_Checksum :: ObjectAttributes
pattern ObjectAttributes_Checksum = ObjectAttributes' "Checksum"

pattern ObjectAttributes_ETag :: ObjectAttributes
pattern ObjectAttributes_ETag = ObjectAttributes' "ETag"

pattern ObjectAttributes_ObjectParts :: ObjectAttributes
pattern ObjectAttributes_ObjectParts = ObjectAttributes' "ObjectParts"

pattern ObjectAttributes_ObjectSize :: ObjectAttributes
pattern ObjectAttributes_ObjectSize = ObjectAttributes' "ObjectSize"

pattern ObjectAttributes_StorageClass :: ObjectAttributes
pattern ObjectAttributes_StorageClass = ObjectAttributes' "StorageClass"

{-# COMPLETE
  ObjectAttributes_Checksum,
  ObjectAttributes_ETag,
  ObjectAttributes_ObjectParts,
  ObjectAttributes_ObjectSize,
  ObjectAttributes_StorageClass,
  ObjectAttributes'
  #-}
