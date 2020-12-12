{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.GlobalEndpointTokenVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.GlobalEndpointTokenVersion
  ( GlobalEndpointTokenVersion
      ( GlobalEndpointTokenVersion',
        V1Token,
        V2Token
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype GlobalEndpointTokenVersion = GlobalEndpointTokenVersion' Lude.Text
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

pattern V1Token :: GlobalEndpointTokenVersion
pattern V1Token = GlobalEndpointTokenVersion' "v1Token"

pattern V2Token :: GlobalEndpointTokenVersion
pattern V2Token = GlobalEndpointTokenVersion' "v2Token"

{-# COMPLETE
  V1Token,
  V2Token,
  GlobalEndpointTokenVersion'
  #-}
