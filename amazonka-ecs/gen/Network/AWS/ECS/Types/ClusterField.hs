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
-- Module      : Network.AWS.ECS.Types.ClusterField
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ClusterField
  ( ClusterField
      ( ..,
        ClusterField_ATTACHMENTS,
        ClusterField_SETTINGS,
        ClusterField_STATISTICS,
        ClusterField_TAGS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ClusterField = ClusterField'
  { fromClusterField ::
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

pattern ClusterField_ATTACHMENTS :: ClusterField
pattern ClusterField_ATTACHMENTS = ClusterField' "ATTACHMENTS"

pattern ClusterField_SETTINGS :: ClusterField
pattern ClusterField_SETTINGS = ClusterField' "SETTINGS"

pattern ClusterField_STATISTICS :: ClusterField
pattern ClusterField_STATISTICS = ClusterField' "STATISTICS"

pattern ClusterField_TAGS :: ClusterField
pattern ClusterField_TAGS = ClusterField' "TAGS"

{-# COMPLETE
  ClusterField_ATTACHMENTS,
  ClusterField_SETTINGS,
  ClusterField_STATISTICS,
  ClusterField_TAGS,
  ClusterField'
  #-}
