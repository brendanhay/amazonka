-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.ActionOwner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionOwner
  ( ActionOwner
      ( ActionOwner',
        AWS,
        Custom,
        ThirdParty
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ActionOwner = ActionOwner' Lude.Text
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

pattern AWS :: ActionOwner
pattern AWS = ActionOwner' "AWS"

pattern Custom :: ActionOwner
pattern Custom = ActionOwner' "Custom"

pattern ThirdParty :: ActionOwner
pattern ThirdParty = ActionOwner' "ThirdParty"

{-# COMPLETE
  AWS,
  Custom,
  ThirdParty,
  ActionOwner'
  #-}
