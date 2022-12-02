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
-- Module      : Amazonka.MGN.Types.InitiatedBy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.InitiatedBy
  ( InitiatedBy
      ( ..,
        InitiatedBy_DIAGNOSTIC,
        InitiatedBy_START_CUTOVER,
        InitiatedBy_START_TEST,
        InitiatedBy_TERMINATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype InitiatedBy = InitiatedBy'
  { fromInitiatedBy ::
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

pattern InitiatedBy_DIAGNOSTIC :: InitiatedBy
pattern InitiatedBy_DIAGNOSTIC = InitiatedBy' "DIAGNOSTIC"

pattern InitiatedBy_START_CUTOVER :: InitiatedBy
pattern InitiatedBy_START_CUTOVER = InitiatedBy' "START_CUTOVER"

pattern InitiatedBy_START_TEST :: InitiatedBy
pattern InitiatedBy_START_TEST = InitiatedBy' "START_TEST"

pattern InitiatedBy_TERMINATE :: InitiatedBy
pattern InitiatedBy_TERMINATE = InitiatedBy' "TERMINATE"

{-# COMPLETE
  InitiatedBy_DIAGNOSTIC,
  InitiatedBy_START_CUTOVER,
  InitiatedBy_START_TEST,
  InitiatedBy_TERMINATE,
  InitiatedBy'
  #-}
