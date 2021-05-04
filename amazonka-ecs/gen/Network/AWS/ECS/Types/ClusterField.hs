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

import qualified Network.AWS.Prelude as Prelude

newtype ClusterField = ClusterField'
  { fromClusterField ::
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
