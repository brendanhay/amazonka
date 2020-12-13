{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageSortBy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageSortBy
  ( ImageSortBy
      ( ImageSortBy',
        ISBCreationTime,
        ISBLastModifiedTime,
        ISBImageName
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ImageSortBy = ImageSortBy' Lude.Text
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

pattern ISBCreationTime :: ImageSortBy
pattern ISBCreationTime = ImageSortBy' "CREATION_TIME"

pattern ISBLastModifiedTime :: ImageSortBy
pattern ISBLastModifiedTime = ImageSortBy' "LAST_MODIFIED_TIME"

pattern ISBImageName :: ImageSortBy
pattern ISBImageName = ImageSortBy' "IMAGE_NAME"

{-# COMPLETE
  ISBCreationTime,
  ISBLastModifiedTime,
  ISBImageName,
  ImageSortBy'
  #-}
