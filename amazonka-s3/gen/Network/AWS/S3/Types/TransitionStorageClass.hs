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
-- Module      : Network.AWS.S3.Types.TransitionStorageClass
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.TransitionStorageClass
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

import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

newtype TransitionStorageClass = TransitionStorageClass'
  { fromTransitionStorageClass ::
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
