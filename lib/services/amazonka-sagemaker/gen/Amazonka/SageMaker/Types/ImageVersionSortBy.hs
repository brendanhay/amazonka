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
-- Module      : Amazonka.SageMaker.Types.ImageVersionSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageVersionSortBy
  ( ImageVersionSortBy
      ( ..,
        ImageVersionSortBy_CREATION_TIME,
        ImageVersionSortBy_LAST_MODIFIED_TIME,
        ImageVersionSortBy_VERSION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ImageVersionSortBy = ImageVersionSortBy'
  { fromImageVersionSortBy ::
      Data.Text
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
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
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
