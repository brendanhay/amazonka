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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype LaunchTemplateErrorCode = LaunchTemplateErrorCode'
  { fromLaunchTemplateErrorCode ::
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
