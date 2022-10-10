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
-- Module      : Amazonka.Inspector2.Types.AwsEcrContainerSortBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AwsEcrContainerSortBy
  ( AwsEcrContainerSortBy
      ( ..,
        AwsEcrContainerSortBy_ALL,
        AwsEcrContainerSortBy_CRITICAL,
        AwsEcrContainerSortBy_HIGH
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype AwsEcrContainerSortBy = AwsEcrContainerSortBy'
  { fromAwsEcrContainerSortBy ::
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

pattern AwsEcrContainerSortBy_ALL :: AwsEcrContainerSortBy
pattern AwsEcrContainerSortBy_ALL = AwsEcrContainerSortBy' "ALL"

pattern AwsEcrContainerSortBy_CRITICAL :: AwsEcrContainerSortBy
pattern AwsEcrContainerSortBy_CRITICAL = AwsEcrContainerSortBy' "CRITICAL"

pattern AwsEcrContainerSortBy_HIGH :: AwsEcrContainerSortBy
pattern AwsEcrContainerSortBy_HIGH = AwsEcrContainerSortBy' "HIGH"

{-# COMPLETE
  AwsEcrContainerSortBy_ALL,
  AwsEcrContainerSortBy_CRITICAL,
  AwsEcrContainerSortBy_HIGH,
  AwsEcrContainerSortBy'
  #-}
