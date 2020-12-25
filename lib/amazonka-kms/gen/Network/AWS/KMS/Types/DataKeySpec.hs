{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.DataKeySpec
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.DataKeySpec
  ( DataKeySpec
      ( DataKeySpec',
        DataKeySpecAes256,
        DataKeySpecAes128,
        fromDataKeySpec
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DataKeySpec = DataKeySpec' {fromDataKeySpec :: Core.Text}
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

pattern DataKeySpecAes256 :: DataKeySpec
pattern DataKeySpecAes256 = DataKeySpec' "AES_256"

pattern DataKeySpecAes128 :: DataKeySpec
pattern DataKeySpecAes128 = DataKeySpec' "AES_128"

{-# COMPLETE
  DataKeySpecAes256,
  DataKeySpecAes128,
  DataKeySpec'
  #-}
