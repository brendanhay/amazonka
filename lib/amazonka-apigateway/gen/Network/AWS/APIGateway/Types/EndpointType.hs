{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.EndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.EndpointType
  ( EndpointType
      ( EndpointType',
        Edge,
        Private,
        Regional
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The endpoint type. The valid values are @EDGE@ for edge-optimized API setup, most suitable for mobile applications; @REGIONAL@ for regional API endpoint setup, most suitable for calling from AWS Region; and @PRIVATE@ for private APIs.
newtype EndpointType = EndpointType' Lude.Text
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

pattern Edge :: EndpointType
pattern Edge = EndpointType' "EDGE"

pattern Private :: EndpointType
pattern Private = EndpointType' "PRIVATE"

pattern Regional :: EndpointType
pattern Regional = EndpointType' "REGIONAL"

{-# COMPLETE
  Edge,
  Private,
  Regional,
  EndpointType'
  #-}
