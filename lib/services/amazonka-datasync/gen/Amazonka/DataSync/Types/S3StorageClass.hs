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
-- Module      : Amazonka.DataSync.Types.S3StorageClass
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.S3StorageClass
  ( S3StorageClass
      ( ..,
        S3StorageClass_DEEP_ARCHIVE,
        S3StorageClass_GLACIER,
        S3StorageClass_INTELLIGENT_TIERING,
        S3StorageClass_ONEZONE_IA,
        S3StorageClass_OUTPOSTS,
        S3StorageClass_STANDARD,
        S3StorageClass_STANDARD_IA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype S3StorageClass = S3StorageClass'
  { fromS3StorageClass ::
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

pattern S3StorageClass_DEEP_ARCHIVE :: S3StorageClass
pattern S3StorageClass_DEEP_ARCHIVE = S3StorageClass' "DEEP_ARCHIVE"

pattern S3StorageClass_GLACIER :: S3StorageClass
pattern S3StorageClass_GLACIER = S3StorageClass' "GLACIER"

pattern S3StorageClass_INTELLIGENT_TIERING :: S3StorageClass
pattern S3StorageClass_INTELLIGENT_TIERING = S3StorageClass' "INTELLIGENT_TIERING"

pattern S3StorageClass_ONEZONE_IA :: S3StorageClass
pattern S3StorageClass_ONEZONE_IA = S3StorageClass' "ONEZONE_IA"

pattern S3StorageClass_OUTPOSTS :: S3StorageClass
pattern S3StorageClass_OUTPOSTS = S3StorageClass' "OUTPOSTS"

pattern S3StorageClass_STANDARD :: S3StorageClass
pattern S3StorageClass_STANDARD = S3StorageClass' "STANDARD"

pattern S3StorageClass_STANDARD_IA :: S3StorageClass
pattern S3StorageClass_STANDARD_IA = S3StorageClass' "STANDARD_IA"

{-# COMPLETE
  S3StorageClass_DEEP_ARCHIVE,
  S3StorageClass_GLACIER,
  S3StorageClass_INTELLIGENT_TIERING,
  S3StorageClass_ONEZONE_IA,
  S3StorageClass_OUTPOSTS,
  S3StorageClass_STANDARD,
  S3StorageClass_STANDARD_IA,
  S3StorageClass'
  #-}
