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
-- Module      : Amazonka.MacieV2.Types.UnavailabilityReasonCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.UnavailabilityReasonCode
  ( UnavailabilityReasonCode
      ( ..,
        UnavailabilityReasonCode_INVALID_CLASSIFICATION_RESULT,
        UnavailabilityReasonCode_OBJECT_EXCEEDS_SIZE_QUOTA,
        UnavailabilityReasonCode_OBJECT_UNAVAILABLE,
        UnavailabilityReasonCode_UNSUPPORTED_FINDING_TYPE,
        UnavailabilityReasonCode_UNSUPPORTED_OBJECT_TYPE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies why occurrences of sensitive data can\'t be retrieved for a
-- finding. Possible values are:
newtype UnavailabilityReasonCode = UnavailabilityReasonCode'
  { fromUnavailabilityReasonCode ::
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

pattern UnavailabilityReasonCode_INVALID_CLASSIFICATION_RESULT :: UnavailabilityReasonCode
pattern UnavailabilityReasonCode_INVALID_CLASSIFICATION_RESULT = UnavailabilityReasonCode' "INVALID_CLASSIFICATION_RESULT"

pattern UnavailabilityReasonCode_OBJECT_EXCEEDS_SIZE_QUOTA :: UnavailabilityReasonCode
pattern UnavailabilityReasonCode_OBJECT_EXCEEDS_SIZE_QUOTA = UnavailabilityReasonCode' "OBJECT_EXCEEDS_SIZE_QUOTA"

pattern UnavailabilityReasonCode_OBJECT_UNAVAILABLE :: UnavailabilityReasonCode
pattern UnavailabilityReasonCode_OBJECT_UNAVAILABLE = UnavailabilityReasonCode' "OBJECT_UNAVAILABLE"

pattern UnavailabilityReasonCode_UNSUPPORTED_FINDING_TYPE :: UnavailabilityReasonCode
pattern UnavailabilityReasonCode_UNSUPPORTED_FINDING_TYPE = UnavailabilityReasonCode' "UNSUPPORTED_FINDING_TYPE"

pattern UnavailabilityReasonCode_UNSUPPORTED_OBJECT_TYPE :: UnavailabilityReasonCode
pattern UnavailabilityReasonCode_UNSUPPORTED_OBJECT_TYPE = UnavailabilityReasonCode' "UNSUPPORTED_OBJECT_TYPE"

{-# COMPLETE
  UnavailabilityReasonCode_INVALID_CLASSIFICATION_RESULT,
  UnavailabilityReasonCode_OBJECT_EXCEEDS_SIZE_QUOTA,
  UnavailabilityReasonCode_OBJECT_UNAVAILABLE,
  UnavailabilityReasonCode_UNSUPPORTED_FINDING_TYPE,
  UnavailabilityReasonCode_UNSUPPORTED_OBJECT_TYPE,
  UnavailabilityReasonCode'
  #-}
