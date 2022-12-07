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
-- Module      : Amazonka.CodeBuild.Types.SharedResourceSortByType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.SharedResourceSortByType
  ( SharedResourceSortByType
      ( ..,
        SharedResourceSortByType_ARN,
        SharedResourceSortByType_MODIFIED_TIME
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SharedResourceSortByType = SharedResourceSortByType'
  { fromSharedResourceSortByType ::
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

pattern SharedResourceSortByType_ARN :: SharedResourceSortByType
pattern SharedResourceSortByType_ARN = SharedResourceSortByType' "ARN"

pattern SharedResourceSortByType_MODIFIED_TIME :: SharedResourceSortByType
pattern SharedResourceSortByType_MODIFIED_TIME = SharedResourceSortByType' "MODIFIED_TIME"

{-# COMPLETE
  SharedResourceSortByType_ARN,
  SharedResourceSortByType_MODIFIED_TIME,
  SharedResourceSortByType'
  #-}
