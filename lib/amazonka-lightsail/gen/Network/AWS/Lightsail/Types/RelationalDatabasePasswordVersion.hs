{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.RelationalDatabasePasswordVersion
  ( RelationalDatabasePasswordVersion
      ( RelationalDatabasePasswordVersion',
        RDPVCurrent,
        RDPVPrevious,
        RDPVPending
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RelationalDatabasePasswordVersion = RelationalDatabasePasswordVersion' Lude.Text
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

pattern RDPVCurrent :: RelationalDatabasePasswordVersion
pattern RDPVCurrent = RelationalDatabasePasswordVersion' "CURRENT"

pattern RDPVPrevious :: RelationalDatabasePasswordVersion
pattern RDPVPrevious = RelationalDatabasePasswordVersion' "PREVIOUS"

pattern RDPVPending :: RelationalDatabasePasswordVersion
pattern RDPVPending = RelationalDatabasePasswordVersion' "PENDING"

{-# COMPLETE
  RDPVCurrent,
  RDPVPrevious,
  RDPVPending,
  RelationalDatabasePasswordVersion'
  #-}
