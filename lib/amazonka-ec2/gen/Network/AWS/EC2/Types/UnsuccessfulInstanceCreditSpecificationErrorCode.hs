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
        IncorrectInstanceState,
        InstanceCreditSpecification_NotSupported,
        InvalidInstanceId_Malformed,
        InvalidInstanceId_NotFound
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype UnsuccessfulInstanceCreditSpecificationErrorCode = UnsuccessfulInstanceCreditSpecificationErrorCode' Lude.Text
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

pattern IncorrectInstanceState :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern IncorrectInstanceState = UnsuccessfulInstanceCreditSpecificationErrorCode' "IncorrectInstanceState"

pattern InstanceCreditSpecification_NotSupported :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern InstanceCreditSpecification_NotSupported = UnsuccessfulInstanceCreditSpecificationErrorCode' "InstanceCreditSpecification.NotSupported"

pattern InvalidInstanceId_Malformed :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern InvalidInstanceId_Malformed = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.Malformed"

pattern InvalidInstanceId_NotFound :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern InvalidInstanceId_NotFound = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.NotFound"

{-# COMPLETE
  IncorrectInstanceState,
  InstanceCreditSpecification_NotSupported,
  InvalidInstanceId_Malformed,
  InvalidInstanceId_NotFound,
  UnsuccessfulInstanceCreditSpecificationErrorCode'
  #-}
