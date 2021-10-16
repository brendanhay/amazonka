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

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype UnsuccessfulInstanceCreditSpecificationErrorCode = UnsuccessfulInstanceCreditSpecificationErrorCode'
  { fromUnsuccessfulInstanceCreditSpecificationErrorCode ::
      Core.Text
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
