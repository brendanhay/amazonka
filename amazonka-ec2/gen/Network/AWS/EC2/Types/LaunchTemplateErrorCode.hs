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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateErrorCode
  ( LaunchTemplateErrorCode
      ( ..,
        LaunchTemplateErrorCode_LaunchTemplateIdDoesNotExist,
        LaunchTemplateErrorCode_LaunchTemplateIdMalformed,
        LaunchTemplateErrorCode_LaunchTemplateNameDoesNotExist,
        LaunchTemplateErrorCode_LaunchTemplateNameMalformed,
        LaunchTemplateErrorCode_LaunchTemplateVersionDoesNotExist,
        LaunchTemplateErrorCode_UnexpectedError
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype LaunchTemplateErrorCode = LaunchTemplateErrorCode'
  { fromLaunchTemplateErrorCode ::
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

pattern LaunchTemplateErrorCode_LaunchTemplateIdDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_LaunchTemplateIdDoesNotExist = LaunchTemplateErrorCode' "launchTemplateIdDoesNotExist"

pattern LaunchTemplateErrorCode_LaunchTemplateIdMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_LaunchTemplateIdMalformed = LaunchTemplateErrorCode' "launchTemplateIdMalformed"

pattern LaunchTemplateErrorCode_LaunchTemplateNameDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_LaunchTemplateNameDoesNotExist = LaunchTemplateErrorCode' "launchTemplateNameDoesNotExist"

pattern LaunchTemplateErrorCode_LaunchTemplateNameMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_LaunchTemplateNameMalformed = LaunchTemplateErrorCode' "launchTemplateNameMalformed"

pattern LaunchTemplateErrorCode_LaunchTemplateVersionDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_LaunchTemplateVersionDoesNotExist = LaunchTemplateErrorCode' "launchTemplateVersionDoesNotExist"

pattern LaunchTemplateErrorCode_UnexpectedError :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCode_UnexpectedError = LaunchTemplateErrorCode' "unexpectedError"

{-# COMPLETE
  LaunchTemplateErrorCode_LaunchTemplateIdDoesNotExist,
  LaunchTemplateErrorCode_LaunchTemplateIdMalformed,
  LaunchTemplateErrorCode_LaunchTemplateNameDoesNotExist,
  LaunchTemplateErrorCode_LaunchTemplateNameMalformed,
  LaunchTemplateErrorCode_LaunchTemplateVersionDoesNotExist,
  LaunchTemplateErrorCode_UnexpectedError,
  LaunchTemplateErrorCode'
  #-}
