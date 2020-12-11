-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.FirelensConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.FirelensConfigurationType
  ( FirelensConfigurationType
      ( FirelensConfigurationType',
        Fluentbit,
        Fluentd
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype FirelensConfigurationType = FirelensConfigurationType' Lude.Text
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

pattern Fluentbit :: FirelensConfigurationType
pattern Fluentbit = FirelensConfigurationType' "fluentbit"

pattern Fluentd :: FirelensConfigurationType
pattern Fluentd = FirelensConfigurationType' "fluentd"

{-# COMPLETE
  Fluentbit,
  Fluentd,
  FirelensConfigurationType'
  #-}
