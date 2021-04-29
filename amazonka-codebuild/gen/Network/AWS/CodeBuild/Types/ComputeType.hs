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

import qualified Network.AWS.Prelude as Prelude

newtype ComputeType = ComputeType'
  { fromComputeType ::
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
