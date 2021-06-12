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
-- Module      : Network.AWS.CodeCommit.Types.ReplacementTypeEnum
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ReplacementTypeEnum
  ( ReplacementTypeEnum
      ( ..,
        ReplacementTypeEnum_KEEP_BASE,
        ReplacementTypeEnum_KEEP_DESTINATION,
        ReplacementTypeEnum_KEEP_SOURCE,
        ReplacementTypeEnum_USE_NEW_CONTENT
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ReplacementTypeEnum = ReplacementTypeEnum'
  { fromReplacementTypeEnum ::
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

pattern ReplacementTypeEnum_KEEP_BASE :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_BASE = ReplacementTypeEnum' "KEEP_BASE"

pattern ReplacementTypeEnum_KEEP_DESTINATION :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_DESTINATION = ReplacementTypeEnum' "KEEP_DESTINATION"

pattern ReplacementTypeEnum_KEEP_SOURCE :: ReplacementTypeEnum
pattern ReplacementTypeEnum_KEEP_SOURCE = ReplacementTypeEnum' "KEEP_SOURCE"

pattern ReplacementTypeEnum_USE_NEW_CONTENT :: ReplacementTypeEnum
pattern ReplacementTypeEnum_USE_NEW_CONTENT = ReplacementTypeEnum' "USE_NEW_CONTENT"

{-# COMPLETE
  ReplacementTypeEnum_KEEP_BASE,
  ReplacementTypeEnum_KEEP_DESTINATION,
  ReplacementTypeEnum_KEEP_SOURCE,
  ReplacementTypeEnum_USE_NEW_CONTENT,
  ReplacementTypeEnum'
  #-}
