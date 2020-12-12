{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkerType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkerType
  ( WorkerType
      ( WorkerType',
        G_1X,
        G_2X,
        Standard
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype WorkerType = WorkerType' Lude.Text
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

pattern G_1X :: WorkerType
pattern G_1X = WorkerType' "G.1X"

pattern G_2X :: WorkerType
pattern G_2X = WorkerType' "G.2X"

pattern Standard :: WorkerType
pattern Standard = WorkerType' "Standard"

{-# COMPLETE
  G_1X,
  G_2X,
  Standard,
  WorkerType'
  #-}
