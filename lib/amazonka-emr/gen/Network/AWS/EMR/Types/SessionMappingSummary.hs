{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SessionMappingSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingSummary where

import Network.AWS.EMR.Types.IdentityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for an Amazon EMR Studio session mapping. The details do not include the time the session mapping was last modified.
--
--
--
-- /See:/ 'sessionMappingSummary' smart constructor.
data SessionMappingSummary = SessionMappingSummary'
  { _smsCreationTime ::
      !(Maybe POSIX),
    _smsStudioId :: !(Maybe Text),
    _smsIdentityType :: !(Maybe IdentityType),
    _smsIdentityId :: !(Maybe Text),
    _smsSessionPolicyARN :: !(Maybe Text),
    _smsIdentityName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SessionMappingSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smsCreationTime' - The time the session mapping was created.
--
-- * 'smsStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'smsIdentityType' - Specifies whether the identity mapped to the Studio is a user or a group.
--
-- * 'smsIdentityId' - The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
--
-- * 'smsSessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- * 'smsIdentityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
sessionMappingSummary ::
  SessionMappingSummary
sessionMappingSummary =
  SessionMappingSummary'
    { _smsCreationTime = Nothing,
      _smsStudioId = Nothing,
      _smsIdentityType = Nothing,
      _smsIdentityId = Nothing,
      _smsSessionPolicyARN = Nothing,
      _smsIdentityName = Nothing
    }

-- | The time the session mapping was created.
smsCreationTime :: Lens' SessionMappingSummary (Maybe UTCTime)
smsCreationTime = lens _smsCreationTime (\s a -> s {_smsCreationTime = a}) . mapping _Time

-- | The ID of the Amazon EMR Studio.
smsStudioId :: Lens' SessionMappingSummary (Maybe Text)
smsStudioId = lens _smsStudioId (\s a -> s {_smsStudioId = a})

-- | Specifies whether the identity mapped to the Studio is a user or a group.
smsIdentityType :: Lens' SessionMappingSummary (Maybe IdentityType)
smsIdentityType = lens _smsIdentityType (\s a -> s {_smsIdentityType = a})

-- | The globally unique identifier (GUID) of the user or group from the AWS SSO Identity Store.
smsIdentityId :: Lens' SessionMappingSummary (Maybe Text)
smsIdentityId = lens _smsIdentityId (\s a -> s {_smsIdentityId = a})

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
smsSessionPolicyARN :: Lens' SessionMappingSummary (Maybe Text)
smsSessionPolicyARN = lens _smsSessionPolicyARN (\s a -> s {_smsSessionPolicyARN = a})

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
smsIdentityName :: Lens' SessionMappingSummary (Maybe Text)
smsIdentityName = lens _smsIdentityName (\s a -> s {_smsIdentityName = a})

instance FromJSON SessionMappingSummary where
  parseJSON =
    withObject
      "SessionMappingSummary"
      ( \x ->
          SessionMappingSummary'
            <$> (x .:? "CreationTime")
            <*> (x .:? "StudioId")
            <*> (x .:? "IdentityType")
            <*> (x .:? "IdentityId")
            <*> (x .:? "SessionPolicyArn")
            <*> (x .:? "IdentityName")
      )

instance Hashable SessionMappingSummary

instance NFData SessionMappingSummary
