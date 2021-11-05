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
-- Module      : Network.AWS.AuditManager.Types.SourceType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AuditManager.Types.SourceType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype SourceType = SourceType'
  { fromSourceType ::
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
