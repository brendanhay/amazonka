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
-- Module      : Amazonka.CodeBuild.Types.ProjectVisibilityType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ProjectVisibilityType
  ( ProjectVisibilityType
      ( ..,
        ProjectVisibilityType_PRIVATE,
        ProjectVisibilityType_PUBLIC_READ
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

-- | Specifies the visibility of the project\'s builds. Possible values are:
--
-- [PUBLIC_READ]
--     The project builds are visible to the public.
--
-- [PRIVATE]
--     The project builds are not visible to the public.
newtype ProjectVisibilityType = ProjectVisibilityType'
  { fromProjectVisibilityType ::
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

pattern ProjectVisibilityType_PRIVATE :: ProjectVisibilityType
pattern ProjectVisibilityType_PRIVATE = ProjectVisibilityType' "PRIVATE"

pattern ProjectVisibilityType_PUBLIC_READ :: ProjectVisibilityType
pattern ProjectVisibilityType_PUBLIC_READ = ProjectVisibilityType' "PUBLIC_READ"

{-# COMPLETE
  ProjectVisibilityType_PRIVATE,
  ProjectVisibilityType_PUBLIC_READ,
  ProjectVisibilityType'
  #-}
