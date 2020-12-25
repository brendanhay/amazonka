{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.OriginType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.OriginType
  ( OriginType
      ( OriginType',
        OriginTypeAwsKms,
        OriginTypeExternal,
        OriginTypeAwsCloudhsm,
        fromOriginType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype OriginType = OriginType' {fromOriginType :: Core.Text}
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

pattern OriginTypeAwsKms :: OriginType
pattern OriginTypeAwsKms = OriginType' "AWS_KMS"

pattern OriginTypeExternal :: OriginType
pattern OriginTypeExternal = OriginType' "EXTERNAL"

pattern OriginTypeAwsCloudhsm :: OriginType
pattern OriginTypeAwsCloudhsm = OriginType' "AWS_CLOUDHSM"

{-# COMPLETE
  OriginTypeAwsKms,
  OriginTypeExternal,
  OriginTypeAwsCloudhsm,
  OriginType'
  #-}
