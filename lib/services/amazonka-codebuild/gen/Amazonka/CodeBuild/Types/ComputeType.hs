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
-- Module      : Amazonka.CodeBuild.Types.ComputeType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeBuild.Types.ComputeType
  ( ComputeType
      ( ..,
        ComputeType_BUILD_GENERAL1_2XLARGE,
        ComputeType_BUILD_GENERAL1_LARGE,
        ComputeType_BUILD_GENERAL1_MEDIUM,
        ComputeType_BUILD_GENERAL1_SMALL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ComputeType = ComputeType'
  { fromComputeType ::
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

pattern ComputeType_BUILD_GENERAL1_2XLARGE :: ComputeType
pattern ComputeType_BUILD_GENERAL1_2XLARGE = ComputeType' "BUILD_GENERAL1_2XLARGE"

pattern ComputeType_BUILD_GENERAL1_LARGE :: ComputeType
pattern ComputeType_BUILD_GENERAL1_LARGE = ComputeType' "BUILD_GENERAL1_LARGE"

pattern ComputeType_BUILD_GENERAL1_MEDIUM :: ComputeType
pattern ComputeType_BUILD_GENERAL1_MEDIUM = ComputeType' "BUILD_GENERAL1_MEDIUM"

pattern ComputeType_BUILD_GENERAL1_SMALL :: ComputeType
pattern ComputeType_BUILD_GENERAL1_SMALL = ComputeType' "BUILD_GENERAL1_SMALL"

{-# COMPLETE
  ComputeType_BUILD_GENERAL1_2XLARGE,
  ComputeType_BUILD_GENERAL1_LARGE,
  ComputeType_BUILD_GENERAL1_MEDIUM,
  ComputeType_BUILD_GENERAL1_SMALL,
  ComputeType'
  #-}
