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
-- Module      : Network.AWS.MGN.Types.InitiatedBy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.InitiatedBy
  ( InitiatedBy
      ( ..,
        InitiatedBy_DIAGNOSTIC,
        InitiatedBy_START_CUTOVER,
        InitiatedBy_START_TEST,
        InitiatedBy_TERMINATE
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype InitiatedBy = InitiatedBy'
  { fromInitiatedBy ::
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
