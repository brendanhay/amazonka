{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSM.Types.ClientVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSM.Types.ClientVersion
  ( ClientVersion
      ( ClientVersion',
        ClientVersionVD5_1,
        ClientVersionVD5_3,
        fromClientVersion
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ClientVersion = ClientVersion'
  { fromClientVersion ::
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

pattern ClientVersionVD5_1 :: ClientVersion
pattern ClientVersionVD5_1 = ClientVersion' "5.1"

pattern ClientVersionVD5_3 :: ClientVersion
pattern ClientVersionVD5_3 = ClientVersion' "5.3"

{-# COMPLETE
  ClientVersionVD5_1,
  ClientVersionVD5_3,
  ClientVersion'
  #-}
