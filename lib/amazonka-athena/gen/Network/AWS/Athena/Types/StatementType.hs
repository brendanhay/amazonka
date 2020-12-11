-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.StatementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.StatementType
  ( StatementType
      ( StatementType',
        Ddl,
        Dml,
        Utility
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StatementType = StatementType' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern Ddl :: StatementType
pattern Ddl = StatementType' "DDL"

pattern Dml :: StatementType
pattern Dml = StatementType' "DML"

pattern Utility :: StatementType
pattern Utility = StatementType' "UTILITY"

{-# COMPLETE
  Ddl,
  Dml,
  Utility,
  StatementType'
  #-}
