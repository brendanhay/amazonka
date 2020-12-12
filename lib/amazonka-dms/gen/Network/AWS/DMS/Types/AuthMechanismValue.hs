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
        AMVDefault,
        AMVMongodbCr,
        AMVScramSha1
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuthMechanismValue = AuthMechanismValue' Lude.Text
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

pattern AMVDefault :: AuthMechanismValue
pattern AMVDefault = AuthMechanismValue' "default"

pattern AMVMongodbCr :: AuthMechanismValue
pattern AMVMongodbCr = AuthMechanismValue' "mongodb_cr"

pattern AMVScramSha1 :: AuthMechanismValue
pattern AMVScramSha1 = AuthMechanismValue' "scram_sha_1"

{-# COMPLETE
  AMVDefault,
  AMVMongodbCr,
  AMVScramSha1,
  AuthMechanismValue'
  #-}
