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
-- Module      : Amazonka.CodeGuruReviewer.Types.RepositoryAssociationState
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeGuruReviewer.Types.RepositoryAssociationState
  ( RepositoryAssociationState
      ( ..,
        RepositoryAssociationState_Associated,
        RepositoryAssociationState_Associating,
        RepositoryAssociationState_Disassociated,
        RepositoryAssociationState_Disassociating,
        RepositoryAssociationState_Failed
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype RepositoryAssociationState = RepositoryAssociationState'
  { fromRepositoryAssociationState ::
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

pattern RepositoryAssociationState_Associated :: RepositoryAssociationState
pattern RepositoryAssociationState_Associated = RepositoryAssociationState' "Associated"

pattern RepositoryAssociationState_Associating :: RepositoryAssociationState
pattern RepositoryAssociationState_Associating = RepositoryAssociationState' "Associating"

pattern RepositoryAssociationState_Disassociated :: RepositoryAssociationState
pattern RepositoryAssociationState_Disassociated = RepositoryAssociationState' "Disassociated"

pattern RepositoryAssociationState_Disassociating :: RepositoryAssociationState
pattern RepositoryAssociationState_Disassociating = RepositoryAssociationState' "Disassociating"

pattern RepositoryAssociationState_Failed :: RepositoryAssociationState
pattern RepositoryAssociationState_Failed = RepositoryAssociationState' "Failed"

{-# COMPLETE
  RepositoryAssociationState_Associated,
  RepositoryAssociationState_Associating,
  RepositoryAssociationState_Disassociated,
  RepositoryAssociationState_Disassociating,
  RepositoryAssociationState_Failed,
  RepositoryAssociationState'
  #-}
