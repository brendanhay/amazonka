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
-- Module      : Amazonka.CloudFormation.Types.ChangeSetType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.ChangeSetType
  ( ChangeSetType
      ( ..,
        ChangeSetType_CREATE,
        ChangeSetType_IMPORT,
        ChangeSetType_UPDATE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeSetType = ChangeSetType'
  { fromChangeSetType ::
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

pattern ChangeSetType_CREATE :: ChangeSetType
pattern ChangeSetType_CREATE = ChangeSetType' "CREATE"

pattern ChangeSetType_IMPORT :: ChangeSetType
pattern ChangeSetType_IMPORT = ChangeSetType' "IMPORT"

pattern ChangeSetType_UPDATE :: ChangeSetType
pattern ChangeSetType_UPDATE = ChangeSetType' "UPDATE"

{-# COMPLETE
  ChangeSetType_CREATE,
  ChangeSetType_IMPORT,
  ChangeSetType_UPDATE,
  ChangeSetType'
  #-}
