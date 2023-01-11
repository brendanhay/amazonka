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
-- Module      : Amazonka.StorageGateway.Types.SMBSecurityStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.SMBSecurityStrategy
  ( SMBSecurityStrategy
      ( ..,
        SMBSecurityStrategy_ClientSpecified,
        SMBSecurityStrategy_MandatoryEncryption,
        SMBSecurityStrategy_MandatorySigning
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SMBSecurityStrategy = SMBSecurityStrategy'
  { fromSMBSecurityStrategy ::
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

pattern SMBSecurityStrategy_ClientSpecified :: SMBSecurityStrategy
pattern SMBSecurityStrategy_ClientSpecified = SMBSecurityStrategy' "ClientSpecified"

pattern SMBSecurityStrategy_MandatoryEncryption :: SMBSecurityStrategy
pattern SMBSecurityStrategy_MandatoryEncryption = SMBSecurityStrategy' "MandatoryEncryption"

pattern SMBSecurityStrategy_MandatorySigning :: SMBSecurityStrategy
pattern SMBSecurityStrategy_MandatorySigning = SMBSecurityStrategy' "MandatorySigning"

{-# COMPLETE
  SMBSecurityStrategy_ClientSpecified,
  SMBSecurityStrategy_MandatoryEncryption,
  SMBSecurityStrategy_MandatorySigning,
  SMBSecurityStrategy'
  #-}
