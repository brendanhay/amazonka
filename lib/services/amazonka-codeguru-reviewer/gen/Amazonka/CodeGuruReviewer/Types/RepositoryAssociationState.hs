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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RepositoryAssociationState = RepositoryAssociationState'
  { fromRepositoryAssociationState ::
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
