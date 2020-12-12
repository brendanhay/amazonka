{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeliveryMediumType
  ( DeliveryMediumType
      ( DeliveryMediumType',
        DMTEmail,
        DMTSms
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeliveryMediumType = DeliveryMediumType' Lude.Text
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

pattern DMTEmail :: DeliveryMediumType
pattern DMTEmail = DeliveryMediumType' "EMAIL"

pattern DMTSms :: DeliveryMediumType
pattern DMTSms = DeliveryMediumType' "SMS"

{-# COMPLETE
  DMTEmail,
  DMTSms,
  DeliveryMediumType'
  #-}
