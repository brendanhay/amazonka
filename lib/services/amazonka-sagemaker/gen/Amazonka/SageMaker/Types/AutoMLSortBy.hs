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
-- Module      : Amazonka.SageMaker.Types.AutoMLSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.AutoMLSortBy
  ( AutoMLSortBy
      ( ..,
        AutoMLSortBy_CreationTime,
        AutoMLSortBy_Name,
        AutoMLSortBy_Status
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AutoMLSortBy = AutoMLSortBy'
  { fromAutoMLSortBy ::
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

pattern AutoMLSortBy_CreationTime :: AutoMLSortBy
pattern AutoMLSortBy_CreationTime = AutoMLSortBy' "CreationTime"

pattern AutoMLSortBy_Name :: AutoMLSortBy
pattern AutoMLSortBy_Name = AutoMLSortBy' "Name"

pattern AutoMLSortBy_Status :: AutoMLSortBy
pattern AutoMLSortBy_Status = AutoMLSortBy' "Status"

{-# COMPLETE
  AutoMLSortBy_CreationTime,
  AutoMLSortBy_Name,
  AutoMLSortBy_Status,
  AutoMLSortBy'
  #-}
