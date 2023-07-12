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
-- Module      : Amazonka.SecurityLake.Types.SubscriptionProtocolType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SubscriptionProtocolType
  ( SubscriptionProtocolType
      ( ..,
        SubscriptionProtocolType_APP,
        SubscriptionProtocolType_EMAIL,
        SubscriptionProtocolType_EMAIL_JSON,
        SubscriptionProtocolType_FIREHOSE,
        SubscriptionProtocolType_HTTP,
        SubscriptionProtocolType_HTTPS,
        SubscriptionProtocolType_LAMBDA,
        SubscriptionProtocolType_SMS,
        SubscriptionProtocolType_SQS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SubscriptionProtocolType = SubscriptionProtocolType'
  { fromSubscriptionProtocolType ::
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

pattern SubscriptionProtocolType_APP :: SubscriptionProtocolType
pattern SubscriptionProtocolType_APP = SubscriptionProtocolType' "APP"

pattern SubscriptionProtocolType_EMAIL :: SubscriptionProtocolType
pattern SubscriptionProtocolType_EMAIL = SubscriptionProtocolType' "EMAIL"

pattern SubscriptionProtocolType_EMAIL_JSON :: SubscriptionProtocolType
pattern SubscriptionProtocolType_EMAIL_JSON = SubscriptionProtocolType' "EMAIL_JSON"

pattern SubscriptionProtocolType_FIREHOSE :: SubscriptionProtocolType
pattern SubscriptionProtocolType_FIREHOSE = SubscriptionProtocolType' "FIREHOSE"

pattern SubscriptionProtocolType_HTTP :: SubscriptionProtocolType
pattern SubscriptionProtocolType_HTTP = SubscriptionProtocolType' "HTTP"

pattern SubscriptionProtocolType_HTTPS :: SubscriptionProtocolType
pattern SubscriptionProtocolType_HTTPS = SubscriptionProtocolType' "HTTPS"

pattern SubscriptionProtocolType_LAMBDA :: SubscriptionProtocolType
pattern SubscriptionProtocolType_LAMBDA = SubscriptionProtocolType' "LAMBDA"

pattern SubscriptionProtocolType_SMS :: SubscriptionProtocolType
pattern SubscriptionProtocolType_SMS = SubscriptionProtocolType' "SMS"

pattern SubscriptionProtocolType_SQS :: SubscriptionProtocolType
pattern SubscriptionProtocolType_SQS = SubscriptionProtocolType' "SQS"

{-# COMPLETE
  SubscriptionProtocolType_APP,
  SubscriptionProtocolType_EMAIL,
  SubscriptionProtocolType_EMAIL_JSON,
  SubscriptionProtocolType_FIREHOSE,
  SubscriptionProtocolType_HTTP,
  SubscriptionProtocolType_HTTPS,
  SubscriptionProtocolType_LAMBDA,
  SubscriptionProtocolType_SMS,
  SubscriptionProtocolType_SQS,
  SubscriptionProtocolType'
  #-}
