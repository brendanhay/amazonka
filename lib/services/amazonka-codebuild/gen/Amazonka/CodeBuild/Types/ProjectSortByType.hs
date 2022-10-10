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
-- Module      : Amazonka.CodeBuild.Types.ProjectSortByType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectSortByType
  ( ProjectSortByType
      ( ..,
        ProjectSortByType_CREATED_TIME,
        ProjectSortByType_LAST_MODIFIED_TIME,
        ProjectSortByType_NAME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ProjectSortByType = ProjectSortByType'
  { fromProjectSortByType ::
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
