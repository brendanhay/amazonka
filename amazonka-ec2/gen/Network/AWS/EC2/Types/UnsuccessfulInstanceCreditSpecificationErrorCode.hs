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
-- Module      : Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.UnsuccessfulInstanceCreditSpecificationErrorCode
  ( UnsuccessfulInstanceCreditSpecificationErrorCode
      ( ..,
        UnsuccessfulInstanceCreditSpecificationErrorCode_IncorrectInstanceState,
        UnsuccessfulInstanceCreditSpecificationErrorCode_InstanceCreditSpecification_NotSupported,
        UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_Malformed,
        UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_NotFound
      ),
  )
where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype UnsuccessfulInstanceCreditSpecificationErrorCode = UnsuccessfulInstanceCreditSpecificationErrorCode'
  { fromUnsuccessfulInstanceCreditSpecificationErrorCode ::
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

pattern UnsuccessfulInstanceCreditSpecificationErrorCode_IncorrectInstanceState :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCode_IncorrectInstanceState = UnsuccessfulInstanceCreditSpecificationErrorCode' "IncorrectInstanceState"

pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InstanceCreditSpecification_NotSupported :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InstanceCreditSpecification_NotSupported = UnsuccessfulInstanceCreditSpecificationErrorCode' "InstanceCreditSpecification.NotSupported"

pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_Malformed :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_Malformed = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.Malformed"

pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_NotFound :: UnsuccessfulInstanceCreditSpecificationErrorCode
pattern UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_NotFound = UnsuccessfulInstanceCreditSpecificationErrorCode' "InvalidInstanceID.NotFound"

{-# COMPLETE
  UnsuccessfulInstanceCreditSpecificationErrorCode_IncorrectInstanceState,
  UnsuccessfulInstanceCreditSpecificationErrorCode_InstanceCreditSpecification_NotSupported,
  UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_Malformed,
  UnsuccessfulInstanceCreditSpecificationErrorCode_InvalidInstanceID_NotFound,
  UnsuccessfulInstanceCreditSpecificationErrorCode'
  #-}
