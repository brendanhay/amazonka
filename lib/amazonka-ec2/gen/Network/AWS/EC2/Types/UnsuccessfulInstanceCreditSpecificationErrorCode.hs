{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
  ( UnsuccessfulInstanceCreditSpecificationErrorCode
      ( UnsuccessfulInstanceCreditSpecificationErrorCode',
        UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_Malformed,
        UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_NotFound,
        UnsuccessfulInstanceCreditSpecificationErrorCodeIncorrectInstanceState,
        UnsuccessfulInstanceCreditSpecificationErrorCodeInstanceCreditSpecification_NotSupported,
        fromUnsuccessfulInstanceCreditSpecificationErrorCode
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype UnsuccessfulInstanceCreditSpecificationErrorCode = UnsuccessfulInstanceCreditSpecificationErrorCode'
  { fromUnsuccessfulInstanceCreditSpecificationErrorCode ::
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

pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_Malformed :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_Malformed = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.Malformed"

pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_NotFound :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_NotFound = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.NotFound"

pattern UnsuccessfulInstanceCreditSpecificationErrorCodeIncorrectInstanceState :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCodeIncorrectInstanceState = UnsuccessfulInstanceCreditSpecificationErrorCode' "IncorrectInstanceState"

pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInstanceCreditSpecification_NotSupported :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCodeInstanceCreditSpecification_NotSupported = UnsuccessfulInstanceCreditSpecificationErrorCode' "InstanceCreditSpecification.NotSupported"

{-# COMPLETE
  UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_Malformed,
  UnsuccessfulInstanceCreditSpecificationErrorCodeInvalidInstanceID_NotFound,
  UnsuccessfulInstanceCreditSpecificationErrorCodeIncorrectInstanceState,
  UnsuccessfulInstanceCreditSpecificationErrorCodeInstanceCreditSpecification_NotSupported,
  UnsuccessfulInstanceCreditSpecificationErrorCode'
  #-}
