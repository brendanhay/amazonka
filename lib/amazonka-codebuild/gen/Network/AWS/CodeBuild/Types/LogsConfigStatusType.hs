-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.Types.LogsConfigStatusType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeBuild.Types.LogsConfigStatusType
  ( LogsConfigStatusType
      ( LogsConfigStatusType',
        Disabled,
        Enabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype LogsConfigStatusType = LogsConfigStatusType' Lude.Text
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

pattern Disabled :: LogsConfigStatusType
pattern Disabled = LogsConfigStatusType' "DISABLED"

pattern Enabled :: LogsConfigStatusType
pattern Enabled = LogsConfigStatusType' "ENABLED"

{-# COMPLETE
  Disabled,
  Enabled,
  LogsConfigStatusType'
  #-}
