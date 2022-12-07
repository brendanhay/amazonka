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
-- Module      : Amazonka.S3.Types.ObjectStorageClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ObjectStorageClass
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype ObjectStorageClass = ObjectStorageClass'
  { fromObjectStorageClass ::
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
