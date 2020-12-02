{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.ContactMethod
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ContactMethod where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ContactMethodStatus
import Network.AWS.Lightsail.Types.ContactProtocol
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceType
import Network.AWS.Prelude

-- | Describes a contact method.
--
--
-- A contact method is a way to send you notifications. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-notifications Notifications in Amazon Lightsail> .
--
--
-- /See:/ 'contactMethod' smart constructor.
data ContactMethod = ContactMethod'
  { _cmStatus ::
      !(Maybe ContactMethodStatus),
    _cmResourceType :: !(Maybe ResourceType),
    _cmArn :: !(Maybe Text),
    _cmCreatedAt :: !(Maybe POSIX),
    _cmLocation :: !(Maybe ResourceLocation),
    _cmProtocol :: !(Maybe ContactProtocol),
    _cmName :: !(Maybe Text),
    _cmSupportCode :: !(Maybe Text),
    _cmContactEndpoint :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContactMethod' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cmStatus' - The current status of the contact method. A contact method has the following possible status:     * @PendingVerification@ - The contact method has not yet been verified, and the verification has not yet expired.     * @Valid@ - The contact method has been verified.     * @InValid@ - An attempt was made to verify the contact method, but the verification has expired.
--
-- * 'cmResourceType' - The Lightsail resource type (e.g., @ContactMethod@ ).
--
-- * 'cmArn' - The Amazon Resource Name (ARN) of the contact method.
--
-- * 'cmCreatedAt' - The timestamp when the contact method was created.
--
-- * 'cmLocation' - Undocumented member.
--
-- * 'cmProtocol' - The protocol of the contact method, such as email or SMS (text messaging).
--
-- * 'cmName' - The name of the contact method.
--
-- * 'cmSupportCode' - The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
--
-- * 'cmContactEndpoint' - The destination of the contact method, such as an email address or a mobile phone number.
contactMethod ::
  ContactMethod
contactMethod =
  ContactMethod'
    { _cmStatus = Nothing,
      _cmResourceType = Nothing,
      _cmArn = Nothing,
      _cmCreatedAt = Nothing,
      _cmLocation = Nothing,
      _cmProtocol = Nothing,
      _cmName = Nothing,
      _cmSupportCode = Nothing,
      _cmContactEndpoint = Nothing
    }

-- | The current status of the contact method. A contact method has the following possible status:     * @PendingVerification@ - The contact method has not yet been verified, and the verification has not yet expired.     * @Valid@ - The contact method has been verified.     * @InValid@ - An attempt was made to verify the contact method, but the verification has expired.
cmStatus :: Lens' ContactMethod (Maybe ContactMethodStatus)
cmStatus = lens _cmStatus (\s a -> s {_cmStatus = a})

-- | The Lightsail resource type (e.g., @ContactMethod@ ).
cmResourceType :: Lens' ContactMethod (Maybe ResourceType)
cmResourceType = lens _cmResourceType (\s a -> s {_cmResourceType = a})

-- | The Amazon Resource Name (ARN) of the contact method.
cmArn :: Lens' ContactMethod (Maybe Text)
cmArn = lens _cmArn (\s a -> s {_cmArn = a})

-- | The timestamp when the contact method was created.
cmCreatedAt :: Lens' ContactMethod (Maybe UTCTime)
cmCreatedAt = lens _cmCreatedAt (\s a -> s {_cmCreatedAt = a}) . mapping _Time

-- | Undocumented member.
cmLocation :: Lens' ContactMethod (Maybe ResourceLocation)
cmLocation = lens _cmLocation (\s a -> s {_cmLocation = a})

-- | The protocol of the contact method, such as email or SMS (text messaging).
cmProtocol :: Lens' ContactMethod (Maybe ContactProtocol)
cmProtocol = lens _cmProtocol (\s a -> s {_cmProtocol = a})

-- | The name of the contact method.
cmName :: Lens' ContactMethod (Maybe Text)
cmName = lens _cmName (\s a -> s {_cmName = a})

-- | The support code. Include this code in your email to support when you have questions about your Lightsail contact method. This code enables our support team to look up your Lightsail information more easily.
cmSupportCode :: Lens' ContactMethod (Maybe Text)
cmSupportCode = lens _cmSupportCode (\s a -> s {_cmSupportCode = a})

-- | The destination of the contact method, such as an email address or a mobile phone number.
cmContactEndpoint :: Lens' ContactMethod (Maybe Text)
cmContactEndpoint = lens _cmContactEndpoint (\s a -> s {_cmContactEndpoint = a})

instance FromJSON ContactMethod where
  parseJSON =
    withObject
      "ContactMethod"
      ( \x ->
          ContactMethod'
            <$> (x .:? "status")
            <*> (x .:? "resourceType")
            <*> (x .:? "arn")
            <*> (x .:? "createdAt")
            <*> (x .:? "location")
            <*> (x .:? "protocol")
            <*> (x .:? "name")
            <*> (x .:? "supportCode")
            <*> (x .:? "contactEndpoint")
      )

instance Hashable ContactMethod

instance NFData ContactMethod
