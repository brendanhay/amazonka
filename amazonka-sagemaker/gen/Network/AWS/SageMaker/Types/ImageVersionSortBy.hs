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
-- Module      : Network.AWS.SageMaker.Types.ImageVersionSortBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ImageVersionSortBy
  ( ImageVersionSortBy
      ( ..,
        ImageVersionSortBy_CREATION_TIME,
        ImageVersionSortBy_LAST_MODIFIED_TIME,
        ImageVersionSortBy_VERSION
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype ImageVersionSortBy = ImageVersionSortBy'
  { fromImageVersionSortBy ::
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

pattern ImageVersionSortBy_CREATION_TIME :: ImageVersionSortBy
pattern ImageVersionSortBy_CREATION_TIME = ImageVersionSortBy' "CREATION_TIME"

pattern ImageVersionSortBy_LAST_MODIFIED_TIME :: ImageVersionSortBy
pattern ImageVersionSortBy_LAST_MODIFIED_TIME = ImageVersionSortBy' "LAST_MODIFIED_TIME"

pattern ImageVersionSortBy_VERSION :: ImageVersionSortBy
pattern ImageVersionSortBy_VERSION = ImageVersionSortBy' "VERSION"

{-# COMPLETE
  ImageVersionSortBy_CREATION_TIME,
  ImageVersionSortBy_LAST_MODIFIED_TIME,
  ImageVersionSortBy_VERSION,
  ImageVersionSortBy'
  #-}
