{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ChangeTokenStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ChangeTokenStatus
  ( ChangeTokenStatus
      ( ChangeTokenStatus',
        Insync,
        Pending,
        Provisioned
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ChangeTokenStatus = ChangeTokenStatus' Lude.Text
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

pattern Insync :: ChangeTokenStatus
pattern Insync = ChangeTokenStatus' "INSYNC"

pattern Pending :: ChangeTokenStatus
pattern Pending = ChangeTokenStatus' "PENDING"

pattern Provisioned :: ChangeTokenStatus
pattern Provisioned = ChangeTokenStatus' "PROVISIONED"

{-# COMPLETE
  Insync,
  Pending,
  Provisioned,
  ChangeTokenStatus'
  #-}
