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
-- Module      : Amazonka.SageMaker.Types.HubContentSortBy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.HubContentSortBy
  ( HubContentSortBy
      ( ..,
        HubContentSortBy_CreationTime,
        HubContentSortBy_HubContentName,
        HubContentSortBy_HubContentStatus
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype HubContentSortBy = HubContentSortBy'
  { fromHubContentSortBy ::
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

pattern HubContentSortBy_CreationTime :: HubContentSortBy
pattern HubContentSortBy_CreationTime = HubContentSortBy' "CreationTime"

pattern HubContentSortBy_HubContentName :: HubContentSortBy
pattern HubContentSortBy_HubContentName = HubContentSortBy' "HubContentName"

pattern HubContentSortBy_HubContentStatus :: HubContentSortBy
pattern HubContentSortBy_HubContentStatus = HubContentSortBy' "HubContentStatus"

{-# COMPLETE
  HubContentSortBy_CreationTime,
  HubContentSortBy_HubContentName,
  HubContentSortBy_HubContentStatus,
  HubContentSortBy'
  #-}
