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
-- Module      : Amazonka.WorkMail.Types.RetentionAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkMail.Types.RetentionAction
  ( RetentionAction
      ( ..,
        RetentionAction_DELETE,
        RetentionAction_NONE,
        RetentionAction_PERMANENTLY_DELETE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype RetentionAction = RetentionAction'
  { fromRetentionAction ::
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

pattern RetentionAction_DELETE :: RetentionAction
pattern RetentionAction_DELETE = RetentionAction' "DELETE"

pattern RetentionAction_NONE :: RetentionAction
pattern RetentionAction_NONE = RetentionAction' "NONE"

pattern RetentionAction_PERMANENTLY_DELETE :: RetentionAction
pattern RetentionAction_PERMANENTLY_DELETE = RetentionAction' "PERMANENTLY_DELETE"

{-# COMPLETE
  RetentionAction_DELETE,
  RetentionAction_NONE,
  RetentionAction_PERMANENTLY_DELETE,
  RetentionAction'
  #-}
