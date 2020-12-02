{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.Types.HandshakeResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.HandshakeResource where

import Network.AWS.Lens
import Network.AWS.Organizations.Types.HandshakeResourceType
import Network.AWS.Prelude

-- | Contains additional data that is needed to process a handshake.
--
--
--
-- /See:/ 'handshakeResource' smart constructor.
data HandshakeResource = HandshakeResource'
  { _hrValue ::
      !(Maybe (Sensitive Text)),
    _hrResources :: !(Maybe [HandshakeResource]),
    _hrType :: !(Maybe HandshakeResourceType)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'HandshakeResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'hrValue' - The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
--
-- * 'hrResources' - When needed, contains an additional array of @HandshakeResource@ objects.
--
-- * 'hrType' - The type of information being passed, specifying how the value is to be interpreted by the other party:     * @ACCOUNT@ - Specifies an AWS account ID number.     * @ORGANIZATION@ - Specifies an organization ID number.     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.      * @OWNER_EMAIL@ - Specifies the email address associated with the management account. Included as information about an organization.      * @OWNER_NAME@ - Specifies the name associated with the management account. Included as information about an organization.      * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
handshakeResource ::
  HandshakeResource
handshakeResource =
  HandshakeResource'
    { _hrValue = Nothing,
      _hrResources = Nothing,
      _hrType = Nothing
    }

-- | The information that is passed to the other party in the handshake. The format of the value string must match the requirements of the specified type.
hrValue :: Lens' HandshakeResource (Maybe Text)
hrValue = lens _hrValue (\s a -> s {_hrValue = a}) . mapping _Sensitive

-- | When needed, contains an additional array of @HandshakeResource@ objects.
hrResources :: Lens' HandshakeResource [HandshakeResource]
hrResources = lens _hrResources (\s a -> s {_hrResources = a}) . _Default . _Coerce

-- | The type of information being passed, specifying how the value is to be interpreted by the other party:     * @ACCOUNT@ - Specifies an AWS account ID number.     * @ORGANIZATION@ - Specifies an organization ID number.     * @EMAIL@ - Specifies the email address that is associated with the account that receives the handshake.      * @OWNER_EMAIL@ - Specifies the email address associated with the management account. Included as information about an organization.      * @OWNER_NAME@ - Specifies the name associated with the management account. Included as information about an organization.      * @NOTES@ - Additional text provided by the handshake initiator and intended for the recipient to read.
hrType :: Lens' HandshakeResource (Maybe HandshakeResourceType)
hrType = lens _hrType (\s a -> s {_hrType = a})

instance FromJSON HandshakeResource where
  parseJSON =
    withObject
      "HandshakeResource"
      ( \x ->
          HandshakeResource'
            <$> (x .:? "Value")
            <*> (x .:? "Resources" .!= mempty)
            <*> (x .:? "Type")
      )

instance Hashable HandshakeResource

instance NFData HandshakeResource
