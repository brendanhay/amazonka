{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkDocs.Types.RoleType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkDocs.Types.RoleType
  ( RoleType
      ( RoleType',
        Viewer,
        Contributor,
        Owner,
        Coowner
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype RoleType = RoleType' Lude.Text
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

pattern Viewer :: RoleType
pattern Viewer = RoleType' "VIEWER"

pattern Contributor :: RoleType
pattern Contributor = RoleType' "CONTRIBUTOR"

pattern Owner :: RoleType
pattern Owner = RoleType' "OWNER"

pattern Coowner :: RoleType
pattern Coowner = RoleType' "COOWNER"

{-# COMPLETE
  Viewer,
  Contributor,
  Owner,
  Coowner,
  RoleType'
  #-}
