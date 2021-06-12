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
-- Module      : Network.AWS.DynamoDB.Types.ProjectionType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ProjectionType
  ( ProjectionType
      ( ..,
        ProjectionType_ALL,
        ProjectionType_INCLUDE,
        ProjectionType_KEYS_ONLY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProjectionType = ProjectionType'
  { fromProjectionType ::
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

pattern ProjectionType_ALL :: ProjectionType
pattern ProjectionType_ALL = ProjectionType' "ALL"

pattern ProjectionType_INCLUDE :: ProjectionType
pattern ProjectionType_INCLUDE = ProjectionType' "INCLUDE"

pattern ProjectionType_KEYS_ONLY :: ProjectionType
pattern ProjectionType_KEYS_ONLY = ProjectionType' "KEYS_ONLY"

{-# COMPLETE
  ProjectionType_ALL,
  ProjectionType_INCLUDE,
  ProjectionType_KEYS_ONLY,
  ProjectionType'
  #-}
