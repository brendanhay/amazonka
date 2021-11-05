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
-- Module      : Amazonka.DevOpsGuru.Types.UpdateResourceCollectionAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.UpdateResourceCollectionAction
  ( UpdateResourceCollectionAction
      ( ..,
        UpdateResourceCollectionAction_ADD,
        UpdateResourceCollectionAction_REMOVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype UpdateResourceCollectionAction = UpdateResourceCollectionAction'
  { fromUpdateResourceCollectionAction ::
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

pattern UpdateResourceCollectionAction_ADD :: UpdateResourceCollectionAction
pattern UpdateResourceCollectionAction_ADD = UpdateResourceCollectionAction' "ADD"

pattern UpdateResourceCollectionAction_REMOVE :: UpdateResourceCollectionAction
pattern UpdateResourceCollectionAction_REMOVE = UpdateResourceCollectionAction' "REMOVE"

{-# COMPLETE
  UpdateResourceCollectionAction_ADD,
  UpdateResourceCollectionAction_REMOVE,
  UpdateResourceCollectionAction'
  #-}
