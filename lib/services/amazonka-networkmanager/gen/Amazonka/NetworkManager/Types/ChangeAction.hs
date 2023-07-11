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
-- Module      : Amazonka.NetworkManager.Types.ChangeAction
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.ChangeAction
  ( ChangeAction
      ( ..,
        ChangeAction_ADD,
        ChangeAction_MODIFY,
        ChangeAction_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ChangeAction = ChangeAction'
  { fromChangeAction ::
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

pattern ChangeAction_ADD :: ChangeAction
pattern ChangeAction_ADD = ChangeAction' "ADD"

pattern ChangeAction_MODIFY :: ChangeAction
pattern ChangeAction_MODIFY = ChangeAction' "MODIFY"

pattern ChangeAction_REMOVE :: ChangeAction
pattern ChangeAction_REMOVE = ChangeAction' "REMOVE"

{-# COMPLETE
  ChangeAction_ADD,
  ChangeAction_MODIFY,
  ChangeAction_REMOVE,
  ChangeAction'
  #-}
