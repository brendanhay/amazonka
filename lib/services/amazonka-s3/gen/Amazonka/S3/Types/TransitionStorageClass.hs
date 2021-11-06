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
-- Module      : Amazonka.S3.Types.TransitionStorageClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.TransitionStorageClass
  ( TransitionStorageClass
      ( ..,
        TransitionStorageClass_DEEP_ARCHIVE,
        TransitionStorageClass_GLACIER,
        TransitionStorageClass_INTELLIGENT_TIERING,
        TransitionStorageClass_ONEZONE_IA,
        TransitionStorageClass_STANDARD_IA
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

newtype TransitionStorageClass = TransitionStorageClass'
  { fromTransitionStorageClass ::
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

pattern TransitionStorageClass_DEEP_ARCHIVE :: TransitionStorageClass
pattern TransitionStorageClass_DEEP_ARCHIVE = TransitionStorageClass' "DEEP_ARCHIVE"

pattern TransitionStorageClass_GLACIER :: TransitionStorageClass
pattern TransitionStorageClass_GLACIER = TransitionStorageClass' "GLACIER"

pattern TransitionStorageClass_INTELLIGENT_TIERING :: TransitionStorageClass
pattern TransitionStorageClass_INTELLIGENT_TIERING = TransitionStorageClass' "INTELLIGENT_TIERING"

pattern TransitionStorageClass_ONEZONE_IA :: TransitionStorageClass
pattern TransitionStorageClass_ONEZONE_IA = TransitionStorageClass' "ONEZONE_IA"

pattern TransitionStorageClass_STANDARD_IA :: TransitionStorageClass
pattern TransitionStorageClass_STANDARD_IA = TransitionStorageClass' "STANDARD_IA"

{-# COMPLETE
  TransitionStorageClass_DEEP_ARCHIVE,
  TransitionStorageClass_GLACIER,
  TransitionStorageClass_INTELLIGENT_TIERING,
  TransitionStorageClass_ONEZONE_IA,
  TransitionStorageClass_STANDARD_IA,
  TransitionStorageClass'
  #-}
