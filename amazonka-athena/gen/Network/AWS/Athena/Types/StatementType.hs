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
-- Module      : Network.AWS.Athena.Types.StatementType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.StatementType
  ( StatementType
      ( ..,
        StatementType_DDL,
        StatementType_DML,
        StatementType_UTILITY
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype StatementType = StatementType'
  { fromStatementType ::
      Core.Text
  }
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
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

pattern StatementType_DDL :: StatementType
pattern StatementType_DDL = StatementType' "DDL"

pattern StatementType_DML :: StatementType
pattern StatementType_DML = StatementType' "DML"

pattern StatementType_UTILITY :: StatementType
pattern StatementType_UTILITY = StatementType' "UTILITY"

{-# COMPLETE
  StatementType_DDL,
  StatementType_DML,
  StatementType_UTILITY,
  StatementType'
  #-}
