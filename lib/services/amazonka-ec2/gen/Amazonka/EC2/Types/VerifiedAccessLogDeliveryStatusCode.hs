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
-- Module      : Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VerifiedAccessLogDeliveryStatusCode
  ( VerifiedAccessLogDeliveryStatusCode
      ( ..,
        VerifiedAccessLogDeliveryStatusCode_Failed,
        VerifiedAccessLogDeliveryStatusCode_Success
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype VerifiedAccessLogDeliveryStatusCode = VerifiedAccessLogDeliveryStatusCode'
  { fromVerifiedAccessLogDeliveryStatusCode ::
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

pattern VerifiedAccessLogDeliveryStatusCode_Failed :: VerifiedAccessLogDeliveryStatusCode
pattern VerifiedAccessLogDeliveryStatusCode_Failed = VerifiedAccessLogDeliveryStatusCode' "failed"

pattern VerifiedAccessLogDeliveryStatusCode_Success :: VerifiedAccessLogDeliveryStatusCode
pattern VerifiedAccessLogDeliveryStatusCode_Success = VerifiedAccessLogDeliveryStatusCode' "success"

{-# COMPLETE
  VerifiedAccessLogDeliveryStatusCode_Failed,
  VerifiedAccessLogDeliveryStatusCode_Success,
  VerifiedAccessLogDeliveryStatusCode'
  #-}
