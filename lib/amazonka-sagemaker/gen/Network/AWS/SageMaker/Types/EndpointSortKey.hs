-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.EndpointSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EndpointSortKey
  ( EndpointSortKey
      ( EndpointSortKey',
        ESKCreationTime,
        ESKName,
        ESKStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EndpointSortKey = EndpointSortKey' Lude.Text
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

pattern ESKCreationTime :: EndpointSortKey
pattern ESKCreationTime = EndpointSortKey' "CreationTime"

pattern ESKName :: EndpointSortKey
pattern ESKName = EndpointSortKey' "Name"

pattern ESKStatus :: EndpointSortKey
pattern ESKStatus = EndpointSortKey' "Status"

{-# COMPLETE
  ESKCreationTime,
  ESKName,
  ESKStatus,
  EndpointSortKey'
  #-}
