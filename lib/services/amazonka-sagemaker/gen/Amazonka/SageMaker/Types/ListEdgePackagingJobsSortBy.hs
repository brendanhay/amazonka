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
-- Module      : Amazonka.SageMaker.Types.ListEdgePackagingJobsSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ListEdgePackagingJobsSortBy
  ( ListEdgePackagingJobsSortBy
      ( ..,
        ListEdgePackagingJobsSortBy_CREATION_TIME,
        ListEdgePackagingJobsSortBy_LAST_MODIFIED_TIME,
        ListEdgePackagingJobsSortBy_MODEL_NAME,
        ListEdgePackagingJobsSortBy_NAME,
        ListEdgePackagingJobsSortBy_STATUS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype ListEdgePackagingJobsSortBy = ListEdgePackagingJobsSortBy'
  { fromListEdgePackagingJobsSortBy ::
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

pattern ListEdgePackagingJobsSortBy_CREATION_TIME :: ListEdgePackagingJobsSortBy
pattern ListEdgePackagingJobsSortBy_CREATION_TIME = ListEdgePackagingJobsSortBy' "CREATION_TIME"

pattern ListEdgePackagingJobsSortBy_LAST_MODIFIED_TIME :: ListEdgePackagingJobsSortBy
pattern ListEdgePackagingJobsSortBy_LAST_MODIFIED_TIME = ListEdgePackagingJobsSortBy' "LAST_MODIFIED_TIME"

pattern ListEdgePackagingJobsSortBy_MODEL_NAME :: ListEdgePackagingJobsSortBy
pattern ListEdgePackagingJobsSortBy_MODEL_NAME = ListEdgePackagingJobsSortBy' "MODEL_NAME"

pattern ListEdgePackagingJobsSortBy_NAME :: ListEdgePackagingJobsSortBy
pattern ListEdgePackagingJobsSortBy_NAME = ListEdgePackagingJobsSortBy' "NAME"

pattern ListEdgePackagingJobsSortBy_STATUS :: ListEdgePackagingJobsSortBy
pattern ListEdgePackagingJobsSortBy_STATUS = ListEdgePackagingJobsSortBy' "STATUS"

{-# COMPLETE
  ListEdgePackagingJobsSortBy_CREATION_TIME,
  ListEdgePackagingJobsSortBy_LAST_MODIFIED_TIME,
  ListEdgePackagingJobsSortBy_MODEL_NAME,
  ListEdgePackagingJobsSortBy_NAME,
  ListEdgePackagingJobsSortBy_STATUS,
  ListEdgePackagingJobsSortBy'
  #-}
