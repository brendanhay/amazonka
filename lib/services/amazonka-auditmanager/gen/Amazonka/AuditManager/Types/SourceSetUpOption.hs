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
-- Module      : Amazonka.AuditManager.Types.SourceSetUpOption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.SourceSetUpOption
  ( SourceSetUpOption
      ( ..,
        SourceSetUpOption_Procedural_Controls_Mapping,
        SourceSetUpOption_System_Controls_Mapping
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype SourceSetUpOption = SourceSetUpOption'
  { fromSourceSetUpOption ::
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

pattern SourceSetUpOption_Procedural_Controls_Mapping :: SourceSetUpOption
pattern SourceSetUpOption_Procedural_Controls_Mapping = SourceSetUpOption' "Procedural_Controls_Mapping"

pattern SourceSetUpOption_System_Controls_Mapping :: SourceSetUpOption
pattern SourceSetUpOption_System_Controls_Mapping = SourceSetUpOption' "System_Controls_Mapping"

{-# COMPLETE
  SourceSetUpOption_Procedural_Controls_Mapping,
  SourceSetUpOption_System_Controls_Mapping,
  SourceSetUpOption'
  #-}
