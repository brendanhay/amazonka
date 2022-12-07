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
-- Module      : Amazonka.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateErrorCode
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype LaunchTemplateErrorCode = LaunchTemplateErrorCode'
  { fromLaunchTemplateErrorCode ::
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
