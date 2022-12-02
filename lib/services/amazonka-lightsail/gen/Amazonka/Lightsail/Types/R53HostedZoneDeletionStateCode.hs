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
-- Module      : Amazonka.Lightsail.Types.R53HostedZoneDeletionStateCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.R53HostedZoneDeletionStateCode
  ( R53HostedZoneDeletionStateCode
      ( ..,
        R53HostedZoneDeletionStateCode_FAILED,
        R53HostedZoneDeletionStateCode_PENDING,
        R53HostedZoneDeletionStateCode_STARTED,
        R53HostedZoneDeletionStateCode_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype R53HostedZoneDeletionStateCode = R53HostedZoneDeletionStateCode'
  { fromR53HostedZoneDeletionStateCode ::
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

pattern R53HostedZoneDeletionStateCode_FAILED :: R53HostedZoneDeletionStateCode
pattern R53HostedZoneDeletionStateCode_FAILED = R53HostedZoneDeletionStateCode' "FAILED"

pattern R53HostedZoneDeletionStateCode_PENDING :: R53HostedZoneDeletionStateCode
pattern R53HostedZoneDeletionStateCode_PENDING = R53HostedZoneDeletionStateCode' "PENDING"

pattern R53HostedZoneDeletionStateCode_STARTED :: R53HostedZoneDeletionStateCode
pattern R53HostedZoneDeletionStateCode_STARTED = R53HostedZoneDeletionStateCode' "STARTED"

pattern R53HostedZoneDeletionStateCode_SUCCEEDED :: R53HostedZoneDeletionStateCode
pattern R53HostedZoneDeletionStateCode_SUCCEEDED = R53HostedZoneDeletionStateCode' "SUCCEEDED"

{-# COMPLETE
  R53HostedZoneDeletionStateCode_FAILED,
  R53HostedZoneDeletionStateCode_PENDING,
  R53HostedZoneDeletionStateCode_STARTED,
  R53HostedZoneDeletionStateCode_SUCCEEDED,
  R53HostedZoneDeletionStateCode'
  #-}
