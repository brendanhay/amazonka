{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageVersionSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionSortBy
  ( ImageVersionSortBy
      ( ImageVersionSortBy',
        IVSBCreationTime,
        IVSBLastModifiedTime,
        IVSBVersion
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageVersionSortBy = ImageVersionSortBy' Lude.Text
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

pattern IVSBCreationTime :: ImageVersionSortBy
pattern IVSBCreationTime = ImageVersionSortBy' "CREATION_TIME"

pattern IVSBLastModifiedTime :: ImageVersionSortBy
pattern IVSBLastModifiedTime = ImageVersionSortBy' "LAST_MODIFIED_TIME"

pattern IVSBVersion :: ImageVersionSortBy
pattern IVSBVersion = ImageVersionSortBy' "VERSION"

{-# COMPLETE
  IVSBCreationTime,
  IVSBLastModifiedTime,
  IVSBVersion,
  ImageVersionSortBy'
  #-}
