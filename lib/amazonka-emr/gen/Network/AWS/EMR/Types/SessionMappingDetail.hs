{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.SessionMappingDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.SessionMappingDetail where

import Network.AWS.EMR.Types.IdentityType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Details for an Amazon EMR Studio session mapping including creation time, user or group ID, Studio ID, and so on.
--
--
--
-- /See:/ 'sessionMappingDetail' smart constructor.
data SessionMappingDetail = SessionMappingDetail'
  { _smdCreationTime ::
      !(Maybe POSIX),
    _smdStudioId :: !(Maybe Text),
    _smdLastModifiedTime :: !(Maybe POSIX),
    _smdIdentityType :: !(Maybe IdentityType),
    _smdIdentityId :: !(Maybe Text),
    _smdSessionPolicyARN :: !(Maybe Text),
    _smdIdentityName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SessionMappingDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smdCreationTime' - The time the session mapping was created.
--
-- * 'smdStudioId' - The ID of the Amazon EMR Studio.
--
-- * 'smdLastModifiedTime' - The time the session mapping was last modified.
--
-- * 'smdIdentityType' - Specifies whether the identity mapped to the Studio is a user or a group.
--
-- * 'smdIdentityId' - The globally unique identifier (GUID) of the user or group.
--
-- * 'smdSessionPolicyARN' - The Amazon Resource Name (ARN) of the session policy associated with the user or group.
--
-- * 'smdIdentityName' - The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
sessionMappingDetail ::
  SessionMappingDetail
sessionMappingDetail =
  SessionMappingDetail'
    { _smdCreationTime = Nothing,
      _smdStudioId = Nothing,
      _smdLastModifiedTime = Nothing,
      _smdIdentityType = Nothing,
      _smdIdentityId = Nothing,
      _smdSessionPolicyARN = Nothing,
      _smdIdentityName = Nothing
    }

-- | The time the session mapping was created.
smdCreationTime :: Lens' SessionMappingDetail (Maybe UTCTime)
smdCreationTime = lens _smdCreationTime (\s a -> s {_smdCreationTime = a}) . mapping _Time

-- | The ID of the Amazon EMR Studio.
smdStudioId :: Lens' SessionMappingDetail (Maybe Text)
smdStudioId = lens _smdStudioId (\s a -> s {_smdStudioId = a})

-- | The time the session mapping was last modified.
smdLastModifiedTime :: Lens' SessionMappingDetail (Maybe UTCTime)
smdLastModifiedTime = lens _smdLastModifiedTime (\s a -> s {_smdLastModifiedTime = a}) . mapping _Time

-- | Specifies whether the identity mapped to the Studio is a user or a group.
smdIdentityType :: Lens' SessionMappingDetail (Maybe IdentityType)
smdIdentityType = lens _smdIdentityType (\s a -> s {_smdIdentityType = a})

-- | The globally unique identifier (GUID) of the user or group.
smdIdentityId :: Lens' SessionMappingDetail (Maybe Text)
smdIdentityId = lens _smdIdentityId (\s a -> s {_smdIdentityId = a})

-- | The Amazon Resource Name (ARN) of the session policy associated with the user or group.
smdSessionPolicyARN :: Lens' SessionMappingDetail (Maybe Text)
smdSessionPolicyARN = lens _smdSessionPolicyARN (\s a -> s {_smdSessionPolicyARN = a})

-- | The name of the user or group. For more information, see <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_User.html#singlesignon-Type-User-UserId UserName> and <https://docs.aws.amazon.com/singlesignon/latest/IdentityStoreAPIReference/API_Group.html#singlesignon-Type-Group-DisplayName DisplayName> in the /AWS SSO Identity Store API Reference/ .
smdIdentityName :: Lens' SessionMappingDetail (Maybe Text)
smdIdentityName = lens _smdIdentityName (\s a -> s {_smdIdentityName = a})

instance FromJSON SessionMappingDetail where
  parseJSON =
    withObject
      "SessionMappingDetail"
      ( \x ->
          SessionMappingDetail'
            <$> (x .:? "CreationTime")
            <*> (x .:? "StudioId")
            <*> (x .:? "LastModifiedTime")
            <*> (x .:? "IdentityType")
            <*> (x .:? "IdentityId")
            <*> (x .:? "SessionPolicyArn")
            <*> (x .:? "IdentityName")
      )

instance Hashable SessionMappingDetail

instance NFData SessionMappingDetail
