{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.EC2TagFilterType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.EC2TagFilterType
  ( EC2TagFilterType
      ( EC2TagFilterType',
        EC2TagFilterTypeKeyOnly,
        EC2TagFilterTypeValueOnly,
        EC2TagFilterTypeKeyAndValue,
        fromEC2TagFilterType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype EC2TagFilterType = EC2TagFilterType'
  { fromEC2TagFilterType ::
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

pattern EC2TagFilterTypeKeyOnly :: EC2TagFilterType
pattern EC2TagFilterTypeKeyOnly = EC2TagFilterType' "KEY_ONLY"

pattern EC2TagFilterTypeValueOnly :: EC2TagFilterType
pattern EC2TagFilterTypeValueOnly = EC2TagFilterType' "VALUE_ONLY"

pattern EC2TagFilterTypeKeyAndValue :: EC2TagFilterType
pattern EC2TagFilterTypeKeyAndValue = EC2TagFilterType' "KEY_AND_VALUE"

{-# COMPLETE
  EC2TagFilterTypeKeyOnly,
  EC2TagFilterTypeValueOnly,
  EC2TagFilterTypeKeyAndValue,
  EC2TagFilterType'
  #-}
