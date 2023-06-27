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
-- Module      : Amazonka.OpenSearch.Types.DomainHealth
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.DomainHealth
  ( DomainHealth
      ( ..,
        DomainHealth_Green,
        DomainHealth_NotAvailable,
        DomainHealth_Red,
        DomainHealth_Yellow
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DomainHealth = DomainHealth'
  { fromDomainHealth ::
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

pattern DomainHealth_Green :: DomainHealth
pattern DomainHealth_Green = DomainHealth' "Green"

pattern DomainHealth_NotAvailable :: DomainHealth
pattern DomainHealth_NotAvailable = DomainHealth' "NotAvailable"

pattern DomainHealth_Red :: DomainHealth
pattern DomainHealth_Red = DomainHealth' "Red"

pattern DomainHealth_Yellow :: DomainHealth
pattern DomainHealth_Yellow = DomainHealth' "Yellow"

{-# COMPLETE
  DomainHealth_Green,
  DomainHealth_NotAvailable,
  DomainHealth_Red,
  DomainHealth_Yellow,
  DomainHealth'
  #-}
