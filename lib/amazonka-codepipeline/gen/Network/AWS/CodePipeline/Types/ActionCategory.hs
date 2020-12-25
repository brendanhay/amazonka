{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionCategory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionCategory
  ( ActionCategory
      ( ActionCategory',
        ActionCategorySource,
        ActionCategoryBuild,
        ActionCategoryDeploy,
        ActionCategoryTest,
        ActionCategoryInvoke,
        ActionCategoryApproval,
        fromActionCategory
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ActionCategory = ActionCategory'
  { fromActionCategory ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ActionCategorySource :: ActionCategory
pattern ActionCategorySource = ActionCategory' "Source"

pattern ActionCategoryBuild :: ActionCategory
pattern ActionCategoryBuild = ActionCategory' "Build"

pattern ActionCategoryDeploy :: ActionCategory
pattern ActionCategoryDeploy = ActionCategory' "Deploy"

pattern ActionCategoryTest :: ActionCategory
pattern ActionCategoryTest = ActionCategory' "Test"

pattern ActionCategoryInvoke :: ActionCategory
pattern ActionCategoryInvoke = ActionCategory' "Invoke"

pattern ActionCategoryApproval :: ActionCategory
pattern ActionCategoryApproval = ActionCategory' "Approval"

{-# COMPLETE
  ActionCategorySource,
  ActionCategoryBuild,
  ActionCategoryDeploy,
  ActionCategoryTest,
  ActionCategoryInvoke,
  ActionCategoryApproval,
  ActionCategory'
  #-}
