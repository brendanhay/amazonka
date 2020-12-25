{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
  ( RequiredAttributeBehavior
      ( RequiredAttributeBehavior',
        RequiredAttributeBehaviorRequiredAlways,
        RequiredAttributeBehaviorNotRequired,
        fromRequiredAttributeBehavior
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RequiredAttributeBehavior = RequiredAttributeBehavior'
  { fromRequiredAttributeBehavior ::
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

pattern RequiredAttributeBehaviorRequiredAlways :: RequiredAttributeBehavior
pattern RequiredAttributeBehaviorRequiredAlways = RequiredAttributeBehavior' "REQUIRED_ALWAYS"

pattern RequiredAttributeBehaviorNotRequired :: RequiredAttributeBehavior
pattern RequiredAttributeBehaviorNotRequired = RequiredAttributeBehavior' "NOT_REQUIRED"

{-# COMPLETE
  RequiredAttributeBehaviorRequiredAlways,
  RequiredAttributeBehaviorNotRequired,
  RequiredAttributeBehavior'
  #-}
