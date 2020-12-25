{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationTargetType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationTargetType
  ( RemediationTargetType
      ( RemediationTargetType',
        RemediationTargetTypeSsmDocument,
        fromRemediationTargetType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype RemediationTargetType = RemediationTargetType'
  { fromRemediationTargetType ::
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

pattern RemediationTargetTypeSsmDocument :: RemediationTargetType
pattern RemediationTargetTypeSsmDocument = RemediationTargetType' "SSM_DOCUMENT"

{-# COMPLETE
  RemediationTargetTypeSsmDocument,
  RemediationTargetType'
  #-}
