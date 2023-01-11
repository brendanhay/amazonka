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
-- Module      : Amazonka.Lightsail.Types.NameServersUpdateStateCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.NameServersUpdateStateCode
  ( NameServersUpdateStateCode
      ( ..,
        NameServersUpdateStateCode_FAILED,
        NameServersUpdateStateCode_PENDING,
        NameServersUpdateStateCode_STARTED,
        NameServersUpdateStateCode_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype NameServersUpdateStateCode = NameServersUpdateStateCode'
  { fromNameServersUpdateStateCode ::
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

pattern NameServersUpdateStateCode_FAILED :: NameServersUpdateStateCode
pattern NameServersUpdateStateCode_FAILED = NameServersUpdateStateCode' "FAILED"

pattern NameServersUpdateStateCode_PENDING :: NameServersUpdateStateCode
pattern NameServersUpdateStateCode_PENDING = NameServersUpdateStateCode' "PENDING"

pattern NameServersUpdateStateCode_STARTED :: NameServersUpdateStateCode
pattern NameServersUpdateStateCode_STARTED = NameServersUpdateStateCode' "STARTED"

pattern NameServersUpdateStateCode_SUCCEEDED :: NameServersUpdateStateCode
pattern NameServersUpdateStateCode_SUCCEEDED = NameServersUpdateStateCode' "SUCCEEDED"

{-# COMPLETE
  NameServersUpdateStateCode_FAILED,
  NameServersUpdateStateCode_PENDING,
  NameServersUpdateStateCode_STARTED,
  NameServersUpdateStateCode_SUCCEEDED,
  NameServersUpdateStateCode'
  #-}
