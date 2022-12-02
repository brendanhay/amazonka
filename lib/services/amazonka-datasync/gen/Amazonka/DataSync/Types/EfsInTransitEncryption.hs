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
-- Module      : Amazonka.DataSync.Types.EfsInTransitEncryption
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.EfsInTransitEncryption
  ( EfsInTransitEncryption
      ( ..,
        EfsInTransitEncryption_NONE,
        EfsInTransitEncryption_TLS1_2
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype EfsInTransitEncryption = EfsInTransitEncryption'
  { fromEfsInTransitEncryption ::
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

pattern EfsInTransitEncryption_NONE :: EfsInTransitEncryption
pattern EfsInTransitEncryption_NONE = EfsInTransitEncryption' "NONE"

pattern EfsInTransitEncryption_TLS1_2 :: EfsInTransitEncryption
pattern EfsInTransitEncryption_TLS1_2 = EfsInTransitEncryption' "TLS1_2"

{-# COMPLETE
  EfsInTransitEncryption_NONE,
  EfsInTransitEncryption_TLS1_2,
  EfsInTransitEncryption'
  #-}
