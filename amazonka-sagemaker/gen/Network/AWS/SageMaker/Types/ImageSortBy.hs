{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ImageSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageSortBy
  ( ImageSortBy
      ( ..,
        ImageSortBy_CREATION_TIME,
        ImageSortBy_IMAGE_NAME,
        ImageSortBy_LAST_MODIFIED_TIME
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ImageSortBy = ImageSortBy'
  { fromImageSortBy ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
    )

pattern ImageSortBy_CREATION_TIME :: ImageSortBy
pattern ImageSortBy_CREATION_TIME = ImageSortBy' "CREATION_TIME"

pattern ImageSortBy_IMAGE_NAME :: ImageSortBy
pattern ImageSortBy_IMAGE_NAME = ImageSortBy' "IMAGE_NAME"

pattern ImageSortBy_LAST_MODIFIED_TIME :: ImageSortBy
pattern ImageSortBy_LAST_MODIFIED_TIME = ImageSortBy' "LAST_MODIFIED_TIME"

{-# COMPLETE
  ImageSortBy_CREATION_TIME,
  ImageSortBy_IMAGE_NAME,
  ImageSortBy_LAST_MODIFIED_TIME,
  ImageSortBy'
  #-}
