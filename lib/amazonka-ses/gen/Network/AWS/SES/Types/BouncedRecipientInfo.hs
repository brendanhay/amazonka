{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.BouncedRecipientInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.BouncedRecipientInfo where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.BounceType
import Network.AWS.SES.Types.RecipientDsnFields

-- | Recipient-related information to include in the Delivery Status Notification (DSN) when an email that Amazon SES receives on your behalf bounces.
--
--
-- For information about receiving email through Amazon SES, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'bouncedRecipientInfo' smart constructor.
data BouncedRecipientInfo = BouncedRecipientInfo'
  { _briBounceType ::
      !(Maybe BounceType),
    _briRecipientDsnFields ::
      !(Maybe RecipientDsnFields),
    _briRecipientARN :: !(Maybe Text),
    _briRecipient :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BouncedRecipientInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'briBounceType' - The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
--
-- * 'briRecipientDsnFields' - Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
--
-- * 'briRecipientARN' - This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
--
-- * 'briRecipient' - The email address of the recipient of the bounced email.
bouncedRecipientInfo ::
  -- | 'briRecipient'
  Text ->
  BouncedRecipientInfo
bouncedRecipientInfo pRecipient_ =
  BouncedRecipientInfo'
    { _briBounceType = Nothing,
      _briRecipientDsnFields = Nothing,
      _briRecipientARN = Nothing,
      _briRecipient = pRecipient_
    }

-- | The reason for the bounce. You must provide either this parameter or @RecipientDsnFields@ .
briBounceType :: Lens' BouncedRecipientInfo (Maybe BounceType)
briBounceType = lens _briBounceType (\s a -> s {_briBounceType = a})

-- | Recipient-related DSN fields, most of which would normally be filled in automatically when provided with a @BounceType@ . You must provide either this parameter or @BounceType@ .
briRecipientDsnFields :: Lens' BouncedRecipientInfo (Maybe RecipientDsnFields)
briRecipientDsnFields = lens _briRecipientDsnFields (\s a -> s {_briRecipientDsnFields = a})

-- | This parameter is used only for sending authorization. It is the ARN of the identity that is associated with the sending authorization policy that permits you to receive email for the recipient of the bounced email. For more information about sending authorization, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/sending-authorization.html Amazon SES Developer Guide> .
briRecipientARN :: Lens' BouncedRecipientInfo (Maybe Text)
briRecipientARN = lens _briRecipientARN (\s a -> s {_briRecipientARN = a})

-- | The email address of the recipient of the bounced email.
briRecipient :: Lens' BouncedRecipientInfo Text
briRecipient = lens _briRecipient (\s a -> s {_briRecipient = a})

instance Hashable BouncedRecipientInfo

instance NFData BouncedRecipientInfo

instance ToQuery BouncedRecipientInfo where
  toQuery BouncedRecipientInfo' {..} =
    mconcat
      [ "BounceType" =: _briBounceType,
        "RecipientDsnFields" =: _briRecipientDsnFields,
        "RecipientArn" =: _briRecipientARN,
        "Recipient" =: _briRecipient
      ]
