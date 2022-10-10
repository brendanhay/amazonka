{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Types.ImageSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageSortBy
  ( ImageSortBy
      ( ..,
        ImageSortBy_CREATION_TIME,
        ImageSortBy_IMAGE_NAME,
        ImageSortBy_LAST_MODIFIED_TIME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ImageSortBy = ImageSortBy'
  { fromImageSortBy ::
      Core.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
