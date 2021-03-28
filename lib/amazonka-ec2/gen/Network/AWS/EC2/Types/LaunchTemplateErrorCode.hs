{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateErrorCode
  ( LaunchTemplateErrorCode
    ( LaunchTemplateErrorCode'
    , LaunchTemplateErrorCodeLaunchTemplateIdDoesNotExist
    , LaunchTemplateErrorCodeLaunchTemplateIdMalformed
    , LaunchTemplateErrorCodeLaunchTemplateNameDoesNotExist
    , LaunchTemplateErrorCodeLaunchTemplateNameMalformed
    , LaunchTemplateErrorCodeLaunchTemplateVersionDoesNotExist
    , LaunchTemplateErrorCodeUnexpectedError
    , fromLaunchTemplateErrorCode
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype LaunchTemplateErrorCode = LaunchTemplateErrorCode'{fromLaunchTemplateErrorCode
                                                           :: Core.Text}
                                    deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                    Core.Generic)
                                    deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                      Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                      Core.FromJSON, Core.ToXML, Core.FromXML,
                                                      Core.ToText, Core.FromText, Core.ToByteString,
                                                      Core.ToQuery, Core.ToHeader)

pattern LaunchTemplateErrorCodeLaunchTemplateIdDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeLaunchTemplateIdDoesNotExist = LaunchTemplateErrorCode' "launchTemplateIdDoesNotExist"

pattern LaunchTemplateErrorCodeLaunchTemplateIdMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeLaunchTemplateIdMalformed = LaunchTemplateErrorCode' "launchTemplateIdMalformed"

pattern LaunchTemplateErrorCodeLaunchTemplateNameDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeLaunchTemplateNameDoesNotExist = LaunchTemplateErrorCode' "launchTemplateNameDoesNotExist"

pattern LaunchTemplateErrorCodeLaunchTemplateNameMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeLaunchTemplateNameMalformed = LaunchTemplateErrorCode' "launchTemplateNameMalformed"

pattern LaunchTemplateErrorCodeLaunchTemplateVersionDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeLaunchTemplateVersionDoesNotExist = LaunchTemplateErrorCode' "launchTemplateVersionDoesNotExist"

pattern LaunchTemplateErrorCodeUnexpectedError :: LaunchTemplateErrorCode
pattern LaunchTemplateErrorCodeUnexpectedError = LaunchTemplateErrorCode' "unexpectedError"

{-# COMPLETE 
  LaunchTemplateErrorCodeLaunchTemplateIdDoesNotExist,

  LaunchTemplateErrorCodeLaunchTemplateIdMalformed,

  LaunchTemplateErrorCodeLaunchTemplateNameDoesNotExist,

  LaunchTemplateErrorCodeLaunchTemplateNameMalformed,

  LaunchTemplateErrorCodeLaunchTemplateVersionDoesNotExist,

  LaunchTemplateErrorCodeUnexpectedError,
  LaunchTemplateErrorCode'
  #-}
