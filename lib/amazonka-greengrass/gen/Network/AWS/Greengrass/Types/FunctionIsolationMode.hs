{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.FunctionIsolationMode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.FunctionIsolationMode
  ( FunctionIsolationMode
      ( FunctionIsolationMode',
        FunctionIsolationModeGreengrassContainer,
        FunctionIsolationModeNoContainer,
        fromFunctionIsolationMode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Specifies whether the Lambda function runs in a Greengrass container (default) or without containerization. Unless your scenario requires that you run without containerization, we recommend that you run in a Greengrass container. Omit this value to run the Lambda function with the default containerization for the group.
newtype FunctionIsolationMode = FunctionIsolationMode'
  { fromFunctionIsolationMode ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern FunctionIsolationModeGreengrassContainer :: FunctionIsolationMode
pattern FunctionIsolationModeGreengrassContainer = FunctionIsolationMode' "GreengrassContainer"

pattern FunctionIsolationModeNoContainer :: FunctionIsolationMode
pattern FunctionIsolationModeNoContainer = FunctionIsolationMode' "NoContainer"

{-# COMPLETE
  FunctionIsolationModeGreengrassContainer,
  FunctionIsolationModeNoContainer,
  FunctionIsolationMode'
  #-}
