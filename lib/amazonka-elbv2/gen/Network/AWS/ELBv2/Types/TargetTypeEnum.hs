{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.TargetTypeEnum
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.TargetTypeEnum
  ( TargetTypeEnum
      ( TargetTypeEnum',
        TargetTypeEnumInstance,
        TargetTypeEnumIP,
        TargetTypeEnumLambda,
        fromTargetTypeEnum
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype TargetTypeEnum = TargetTypeEnum'
  { fromTargetTypeEnum ::
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

pattern TargetTypeEnumInstance :: TargetTypeEnum
pattern TargetTypeEnumInstance = TargetTypeEnum' "instance"

pattern TargetTypeEnumIP :: TargetTypeEnum
pattern TargetTypeEnumIP = TargetTypeEnum' "ip"

pattern TargetTypeEnumLambda :: TargetTypeEnum
pattern TargetTypeEnumLambda = TargetTypeEnum' "lambda"

{-# COMPLETE
  TargetTypeEnumInstance,
  TargetTypeEnumIP,
  TargetTypeEnumLambda,
  TargetTypeEnum'
  #-}
