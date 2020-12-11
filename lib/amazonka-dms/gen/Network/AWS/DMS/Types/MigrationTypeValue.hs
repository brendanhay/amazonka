-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.MigrationTypeValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.MigrationTypeValue
  ( MigrationTypeValue
      ( MigrationTypeValue',
        Cdc,
        FullLoad,
        FullLoadAndCdc
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype MigrationTypeValue = MigrationTypeValue' Lude.Text
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

pattern Cdc :: MigrationTypeValue
pattern Cdc = MigrationTypeValue' "cdc"

pattern FullLoad :: MigrationTypeValue
pattern FullLoad = MigrationTypeValue' "full-load"

pattern FullLoadAndCdc :: MigrationTypeValue
pattern FullLoadAndCdc = MigrationTypeValue' "full-load-and-cdc"

{-# COMPLETE
  Cdc,
  FullLoad,
  FullLoadAndCdc,
  MigrationTypeValue'
  #-}
