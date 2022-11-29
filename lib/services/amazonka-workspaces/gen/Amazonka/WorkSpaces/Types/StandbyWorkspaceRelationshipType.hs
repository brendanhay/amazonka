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
-- Module      : Amazonka.WorkSpaces.Types.StandbyWorkspaceRelationshipType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.StandbyWorkspaceRelationshipType
  ( StandbyWorkspaceRelationshipType
      ( ..,
        StandbyWorkspaceRelationshipType_PRIMARY,
        StandbyWorkspaceRelationshipType_STANDBY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype StandbyWorkspaceRelationshipType = StandbyWorkspaceRelationshipType'
  { fromStandbyWorkspaceRelationshipType ::
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

pattern StandbyWorkspaceRelationshipType_PRIMARY :: StandbyWorkspaceRelationshipType
pattern StandbyWorkspaceRelationshipType_PRIMARY = StandbyWorkspaceRelationshipType' "PRIMARY"

pattern StandbyWorkspaceRelationshipType_STANDBY :: StandbyWorkspaceRelationshipType
pattern StandbyWorkspaceRelationshipType_STANDBY = StandbyWorkspaceRelationshipType' "STANDBY"

{-# COMPLETE
  StandbyWorkspaceRelationshipType_PRIMARY,
  StandbyWorkspaceRelationshipType_STANDBY,
  StandbyWorkspaceRelationshipType'
  #-}
