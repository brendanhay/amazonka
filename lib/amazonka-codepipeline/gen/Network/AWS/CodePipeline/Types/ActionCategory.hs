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
        Approval,
        Build,
        Deploy,
        Invoke,
        Source,
        Test
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

pattern Approval :: ActionCategory
pattern Approval = ActionCategory' "Approval"

pattern Build :: ActionCategory
pattern Build = ActionCategory' "Build"

pattern Deploy :: ActionCategory
pattern Deploy = ActionCategory' "Deploy"

pattern Invoke :: ActionCategory
pattern Invoke = ActionCategory' "Invoke"

pattern Source :: ActionCategory
pattern Source = ActionCategory' "Source"

pattern Test :: ActionCategory
pattern Test = ActionCategory' "Test"

{-# COMPLETE
  Approval,
  Build,
  Deploy,
  Invoke,
  Source,
  Test,
  ActionCategory'
  #-}
