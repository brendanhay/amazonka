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
-- Module      : Amazonka.Glue.Types.DeleteBehavior
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.DeleteBehavior
  ( DeleteBehavior
      ( ..,
        DeleteBehavior_DELETE_FROM_DATABASE,
        DeleteBehavior_DEPRECATE_IN_DATABASE,
        DeleteBehavior_LOG
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeleteBehavior = DeleteBehavior'
  { fromDeleteBehavior ::
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

pattern DeleteBehavior_DELETE_FROM_DATABASE :: DeleteBehavior
pattern DeleteBehavior_DELETE_FROM_DATABASE = DeleteBehavior' "DELETE_FROM_DATABASE"

pattern DeleteBehavior_DEPRECATE_IN_DATABASE :: DeleteBehavior
pattern DeleteBehavior_DEPRECATE_IN_DATABASE = DeleteBehavior' "DEPRECATE_IN_DATABASE"

pattern DeleteBehavior_LOG :: DeleteBehavior
pattern DeleteBehavior_LOG = DeleteBehavior' "LOG"

{-# COMPLETE
  DeleteBehavior_DELETE_FROM_DATABASE,
  DeleteBehavior_DEPRECATE_IN_DATABASE,
  DeleteBehavior_LOG,
  DeleteBehavior'
  #-}
