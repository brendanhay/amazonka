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
        Source,
        Build,
        Deploy,
        Test,
        Invoke,
        Approval
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionCategory = ActionCategory' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Source :: ActionCategory
pattern Source = ActionCategory' "Source"

pattern Build :: ActionCategory
pattern Build = ActionCategory' "Build"

pattern Deploy :: ActionCategory
pattern Deploy = ActionCategory' "Deploy"

pattern Test :: ActionCategory
pattern Test = ActionCategory' "Test"

pattern Invoke :: ActionCategory
pattern Invoke = ActionCategory' "Invoke"

pattern Approval :: ActionCategory
pattern Approval = ActionCategory' "Approval"

{-# COMPLETE
  Source,
  Build,
  Deploy,
  Test,
  Invoke,
  Approval,
  ActionCategory'
  #-}
