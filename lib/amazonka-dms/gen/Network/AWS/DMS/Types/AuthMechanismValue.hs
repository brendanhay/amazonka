{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.AuthMechanismValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.AuthMechanismValue
  ( AuthMechanismValue
      ( AuthMechanismValue',
        AuthMechanismValueDefault,
        AuthMechanismValueMongodbCr,
        AuthMechanismValueScramSha1,
        fromAuthMechanismValue
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AuthMechanismValue = AuthMechanismValue'
  { fromAuthMechanismValue ::
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

pattern AuthMechanismValueDefault :: AuthMechanismValue
pattern AuthMechanismValueDefault = AuthMechanismValue' "default"

pattern AuthMechanismValueMongodbCr :: AuthMechanismValue
pattern AuthMechanismValueMongodbCr = AuthMechanismValue' "mongodb_cr"

pattern AuthMechanismValueScramSha1 :: AuthMechanismValue
pattern AuthMechanismValueScramSha1 = AuthMechanismValue' "scram_sha_1"

{-# COMPLETE
  AuthMechanismValueDefault,
  AuthMechanismValueMongodbCr,
  AuthMechanismValueScramSha1,
  AuthMechanismValue'
  #-}
