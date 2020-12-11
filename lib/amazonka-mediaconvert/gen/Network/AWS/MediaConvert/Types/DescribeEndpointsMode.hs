-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.DescribeEndpointsMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.DescribeEndpointsMode
  ( DescribeEndpointsMode
      ( DescribeEndpointsMode',
        DEMDefault,
        DEMGetOnly
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Optional field, defaults to DEFAULT. Specify DEFAULT for this operation to return your endpoints if any exist, or to create an endpoint for you and return it if one doesn't already exist. Specify GET_ONLY to return your endpoints if any exist, or an empty list if none exist.
newtype DescribeEndpointsMode = DescribeEndpointsMode' Lude.Text
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

pattern DEMDefault :: DescribeEndpointsMode
pattern DEMDefault = DescribeEndpointsMode' "DEFAULT"

pattern DEMGetOnly :: DescribeEndpointsMode
pattern DEMGetOnly = DescribeEndpointsMode' "GET_ONLY"

{-# COMPLETE
  DEMDefault,
  DEMGetOnly,
  DescribeEndpointsMode'
  #-}
