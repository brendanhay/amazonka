{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.FileExistsBehavior
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.FileExistsBehavior
  ( FileExistsBehavior
      ( FileExistsBehavior',
        Disallow,
        Overwrite,
        Retain
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FileExistsBehavior = FileExistsBehavior' Lude.Text
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

pattern Disallow :: FileExistsBehavior
pattern Disallow = FileExistsBehavior' "DISALLOW"

pattern Overwrite :: FileExistsBehavior
pattern Overwrite = FileExistsBehavior' "OVERWRITE"

pattern Retain :: FileExistsBehavior
pattern Retain = FileExistsBehavior' "RETAIN"

{-# COMPLETE
  Disallow,
  Overwrite,
  Retain,
  FileExistsBehavior'
  #-}
