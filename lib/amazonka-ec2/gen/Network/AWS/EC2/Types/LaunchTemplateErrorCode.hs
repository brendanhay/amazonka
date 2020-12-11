-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateErrorCode
  ( LaunchTemplateErrorCode
      ( LaunchTemplateErrorCode',
        LaunchTemplateIdDoesNotExist,
        LaunchTemplateIdMalformed,
        LaunchTemplateNameDoesNotExist,
        LaunchTemplateNameMalformed,
        LaunchTemplateVersionDoesNotExist,
        UnexpectedError
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LaunchTemplateErrorCode = LaunchTemplateErrorCode' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern LaunchTemplateIdDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateIdDoesNotExist = LaunchTemplateErrorCode' "launchTemplateIdDoesNotExist"

pattern LaunchTemplateIdMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateIdMalformed = LaunchTemplateErrorCode' "launchTemplateIdMalformed"

pattern LaunchTemplateNameDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateNameDoesNotExist = LaunchTemplateErrorCode' "launchTemplateNameDoesNotExist"

pattern LaunchTemplateNameMalformed :: LaunchTemplateErrorCode
pattern LaunchTemplateNameMalformed = LaunchTemplateErrorCode' "launchTemplateNameMalformed"

pattern LaunchTemplateVersionDoesNotExist :: LaunchTemplateErrorCode
pattern LaunchTemplateVersionDoesNotExist = LaunchTemplateErrorCode' "launchTemplateVersionDoesNotExist"

pattern UnexpectedError :: LaunchTemplateErrorCode
pattern UnexpectedError = LaunchTemplateErrorCode' "unexpectedError"

{-# COMPLETE
  LaunchTemplateIdDoesNotExist,
  LaunchTemplateIdMalformed,
  LaunchTemplateNameDoesNotExist,
  LaunchTemplateNameMalformed,
  LaunchTemplateVersionDoesNotExist,
  UnexpectedError,
  LaunchTemplateErrorCode'
  #-}
