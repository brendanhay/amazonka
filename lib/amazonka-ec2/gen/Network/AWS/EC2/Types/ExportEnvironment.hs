-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ExportEnvironment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ExportEnvironment
  ( ExportEnvironment
      ( ExportEnvironment',
        Citrix,
        Microsoft,
        VMware
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ExportEnvironment = ExportEnvironment' Lude.Text
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

pattern Citrix :: ExportEnvironment
pattern Citrix = ExportEnvironment' "citrix"

pattern Microsoft :: ExportEnvironment
pattern Microsoft = ExportEnvironment' "microsoft"

pattern VMware :: ExportEnvironment
pattern VMware = ExportEnvironment' "vmware"

{-# COMPLETE
  Citrix,
  Microsoft,
  VMware,
  ExportEnvironment'
  #-}
