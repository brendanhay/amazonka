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
-- Module      : Network.AWS.CodeBuild.Types.ComputeType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.ComputeType
  ( ComputeType
      ( ..,
        ComputeType_BUILD_GENERAL1_2XLARGE,
        ComputeType_BUILD_GENERAL1_LARGE,
        ComputeType_BUILD_GENERAL1_MEDIUM,
        ComputeType_BUILD_GENERAL1_SMALL
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype ComputeType = ComputeType'
  { fromComputeType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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
