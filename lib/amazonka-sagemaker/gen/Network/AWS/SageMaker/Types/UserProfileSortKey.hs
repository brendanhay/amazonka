{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserProfileSortKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserProfileSortKey
  ( UserProfileSortKey
      ( UserProfileSortKey',
        UPSKCreationTime,
        UPSKLastModifiedTime
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UserProfileSortKey = UserProfileSortKey' Lude.Text
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

pattern UPSKCreationTime :: UserProfileSortKey
pattern UPSKCreationTime = UserProfileSortKey' "CreationTime"

pattern UPSKLastModifiedTime :: UserProfileSortKey
pattern UPSKLastModifiedTime = UserProfileSortKey' "LastModifiedTime"

{-# COMPLETE
  UPSKCreationTime,
  UPSKLastModifiedTime,
  UserProfileSortKey'
  #-}
