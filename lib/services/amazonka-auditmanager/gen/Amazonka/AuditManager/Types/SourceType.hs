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
-- Module      : Amazonka.AuditManager.Types.SourceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.SourceType
  ( SourceType
      ( ..,
        SourceType_AWS_API_Call,
        SourceType_AWS_Cloudtrail,
        SourceType_AWS_Config,
        SourceType_AWS_Security_Hub,
        SourceType_MANUAL
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
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

pattern SourceType_AWS_API_Call :: SourceType
pattern SourceType_AWS_API_Call = SourceType' "AWS_API_Call"

pattern SourceType_AWS_Cloudtrail :: SourceType
pattern SourceType_AWS_Cloudtrail = SourceType' "AWS_Cloudtrail"

pattern SourceType_AWS_Config :: SourceType
pattern SourceType_AWS_Config = SourceType' "AWS_Config"

pattern SourceType_AWS_Security_Hub :: SourceType
pattern SourceType_AWS_Security_Hub = SourceType' "AWS_Security_Hub"

pattern SourceType_MANUAL :: SourceType
pattern SourceType_MANUAL = SourceType' "MANUAL"

{-# COMPLETE
  SourceType_AWS_API_Call,
  SourceType_AWS_Cloudtrail,
  SourceType_AWS_Config,
  SourceType_AWS_Security_Hub,
  SourceType_MANUAL,
  SourceType'
  #-}
