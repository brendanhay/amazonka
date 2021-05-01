{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DeleteBehavior
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DeleteBehavior
  ( DeleteBehavior
      ( ..,
        DeleteBehavior_DELETE_FROM_DATABASE,
        DeleteBehavior_DEPRECATE_IN_DATABASE,
        DeleteBehavior_LOG
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeleteBehavior = DeleteBehavior'
  { fromDeleteBehavior ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
