-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.EngineFamily
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.EngineFamily
  ( EngineFamily
      ( EngineFamily',
        Mysql,
        Postgresql
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype EngineFamily = EngineFamily' Lude.Text
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

pattern Mysql :: EngineFamily
pattern Mysql = EngineFamily' "MYSQL"

pattern Postgresql :: EngineFamily
pattern Postgresql = EngineFamily' "POSTGRESQL"

{-# COMPLETE
  Mysql,
  Postgresql,
  EngineFamily'
  #-}
