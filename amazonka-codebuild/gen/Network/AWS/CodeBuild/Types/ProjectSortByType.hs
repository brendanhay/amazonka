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
-- Module      : Network.AWS.CodeBuild.Types.ProjectSortByType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ProjectSortByType
  ( ProjectSortByType
      ( ..,
        ProjectSortByType_CREATED_TIME,
        ProjectSortByType_LAST_MODIFIED_TIME,
        ProjectSortByType_NAME
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ProjectSortByType = ProjectSortByType'
  { fromProjectSortByType ::
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

pattern ProjectSortByType_CREATED_TIME :: ProjectSortByType
pattern ProjectSortByType_CREATED_TIME = ProjectSortByType' "CREATED_TIME"

pattern ProjectSortByType_LAST_MODIFIED_TIME :: ProjectSortByType
pattern ProjectSortByType_LAST_MODIFIED_TIME = ProjectSortByType' "LAST_MODIFIED_TIME"

pattern ProjectSortByType_NAME :: ProjectSortByType
pattern ProjectSortByType_NAME = ProjectSortByType' "NAME"

{-# COMPLETE
  ProjectSortByType_CREATED_TIME,
  ProjectSortByType_LAST_MODIFIED_TIME,
  ProjectSortByType_NAME,
  ProjectSortByType'
  #-}
