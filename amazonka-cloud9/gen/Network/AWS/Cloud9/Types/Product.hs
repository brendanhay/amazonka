{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Cloud9.Types.Product where

import Network.AWS.Cloud9.Types.Sum
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about an AWS Cloud9 development environment.
--
--
--
-- /See:/ 'environment' smart constructor.
data Environment = Environment'
  { _eArn         :: !(Maybe Text)
  , _eLifecycle   :: !(Maybe EnvironmentLifecycle)
  , _eOwnerARN    :: !(Maybe Text)
  , _eName        :: !(Maybe Text)
  , _eId          :: !(Maybe Text)
  , _eType        :: !(Maybe EnvironmentType)
  , _eDescription :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Environment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eArn' - The Amazon Resource Name (ARN) of the environment.
--
-- * 'eLifecycle' - The state of the environment in its creation or deletion lifecycle.
--
-- * 'eOwnerARN' - The Amazon Resource Name (ARN) of the environment owner.
--
-- * 'eName' - The name of the environment.
--
-- * 'eId' - The ID of the environment.
--
-- * 'eType' - The type of environment. Valid values include the following:     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.     * @ssh@ : Your own server connects to the environment.
--
-- * 'eDescription' - The description for the environment.
environment
    :: Environment
environment =
  Environment'
    { _eArn = Nothing
    , _eLifecycle = Nothing
    , _eOwnerARN = Nothing
    , _eName = Nothing
    , _eId = Nothing
    , _eType = Nothing
    , _eDescription = Nothing
    }


-- | The Amazon Resource Name (ARN) of the environment.
eArn :: Lens' Environment (Maybe Text)
eArn = lens _eArn (\ s a -> s{_eArn = a})

-- | The state of the environment in its creation or deletion lifecycle.
eLifecycle :: Lens' Environment (Maybe EnvironmentLifecycle)
eLifecycle = lens _eLifecycle (\ s a -> s{_eLifecycle = a})

-- | The Amazon Resource Name (ARN) of the environment owner.
eOwnerARN :: Lens' Environment (Maybe Text)
eOwnerARN = lens _eOwnerARN (\ s a -> s{_eOwnerARN = a})

-- | The name of the environment.
eName :: Lens' Environment (Maybe Text)
eName = lens _eName (\ s a -> s{_eName = a})

-- | The ID of the environment.
eId :: Lens' Environment (Maybe Text)
eId = lens _eId (\ s a -> s{_eId = a})

-- | The type of environment. Valid values include the following:     * @ec2@ : An Amazon Elastic Compute Cloud (Amazon EC2) instance connects to the environment.     * @ssh@ : Your own server connects to the environment.
eType :: Lens' Environment (Maybe EnvironmentType)
eType = lens _eType (\ s a -> s{_eType = a})

-- | The description for the environment.
eDescription :: Lens' Environment (Maybe Text)
eDescription = lens _eDescription (\ s a -> s{_eDescription = a}) . mapping _Sensitive

instance FromJSON Environment where
        parseJSON
          = withObject "Environment"
              (\ x ->
                 Environment' <$>
                   (x .:? "arn") <*> (x .:? "lifecycle") <*>
                     (x .:? "ownerArn")
                     <*> (x .:? "name")
                     <*> (x .:? "id")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable Environment where

instance NFData Environment where

-- | Information about the current creation or deletion lifecycle state of an AWS Cloud9 development environment.
--
--
--
-- /See:/ 'environmentLifecycle' smart constructor.
data EnvironmentLifecycle = EnvironmentLifecycle'
  { _elStatus          :: !(Maybe EnvironmentLifecycleStatus)
  , _elFailureResource :: !(Maybe Text)
  , _elReason          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentLifecycle' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'elStatus' - The current creation or deletion lifecycle state of the environment.     * @CREATED@ : The environment was successfully created.     * @DELETE_FAILED@ : The environment failed to delete.     * @DELETING@ : The environment is in the process of being deleted.
--
-- * 'elFailureResource' - If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
--
-- * 'elReason' - Any informational message about the lifecycle state of the environment.
environmentLifecycle
    :: EnvironmentLifecycle
environmentLifecycle =
  EnvironmentLifecycle'
    {_elStatus = Nothing, _elFailureResource = Nothing, _elReason = Nothing}


-- | The current creation or deletion lifecycle state of the environment.     * @CREATED@ : The environment was successfully created.     * @DELETE_FAILED@ : The environment failed to delete.     * @DELETING@ : The environment is in the process of being deleted.
elStatus :: Lens' EnvironmentLifecycle (Maybe EnvironmentLifecycleStatus)
elStatus = lens _elStatus (\ s a -> s{_elStatus = a})

-- | If the environment failed to delete, the Amazon Resource Name (ARN) of the related AWS resource.
elFailureResource :: Lens' EnvironmentLifecycle (Maybe Text)
elFailureResource = lens _elFailureResource (\ s a -> s{_elFailureResource = a})

-- | Any informational message about the lifecycle state of the environment.
elReason :: Lens' EnvironmentLifecycle (Maybe Text)
elReason = lens _elReason (\ s a -> s{_elReason = a})

instance FromJSON EnvironmentLifecycle where
        parseJSON
          = withObject "EnvironmentLifecycle"
              (\ x ->
                 EnvironmentLifecycle' <$>
                   (x .:? "status") <*> (x .:? "failureResource") <*>
                     (x .:? "reason"))

instance Hashable EnvironmentLifecycle where

instance NFData EnvironmentLifecycle where

-- | Information about an environment member for an AWS Cloud9 development environment.
--
--
--
-- /See:/ 'environmentMember' smart constructor.
data EnvironmentMember = EnvironmentMember'
  { _emLastAccess    :: !(Maybe POSIX)
  , _emUserId        :: !(Maybe Text)
  , _emUserARN       :: !(Maybe Text)
  , _emPermissions   :: !(Maybe Permissions)
  , _emEnvironmentId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnvironmentMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emLastAccess' - The time, expressed in epoch time format, when the environment member last opened the environment.
--
-- * 'emUserId' - The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
--
-- * 'emUserARN' - The Amazon Resource Name (ARN) of the environment member.
--
-- * 'emPermissions' - The type of environment member permissions associated with this environment member. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
--
-- * 'emEnvironmentId' - The ID of the environment for the environment member.
environmentMember
    :: EnvironmentMember
environmentMember =
  EnvironmentMember'
    { _emLastAccess = Nothing
    , _emUserId = Nothing
    , _emUserARN = Nothing
    , _emPermissions = Nothing
    , _emEnvironmentId = Nothing
    }


-- | The time, expressed in epoch time format, when the environment member last opened the environment.
emLastAccess :: Lens' EnvironmentMember (Maybe UTCTime)
emLastAccess = lens _emLastAccess (\ s a -> s{_emLastAccess = a}) . mapping _Time

-- | The user ID in AWS Identity and Access Management (AWS IAM) of the environment member.
emUserId :: Lens' EnvironmentMember (Maybe Text)
emUserId = lens _emUserId (\ s a -> s{_emUserId = a})

-- | The Amazon Resource Name (ARN) of the environment member.
emUserARN :: Lens' EnvironmentMember (Maybe Text)
emUserARN = lens _emUserARN (\ s a -> s{_emUserARN = a})

-- | The type of environment member permissions associated with this environment member. Available values include:     * @owner@ : Owns the environment.     * @read-only@ : Has read-only access to the environment.     * @read-write@ : Has read-write access to the environment.
emPermissions :: Lens' EnvironmentMember (Maybe Permissions)
emPermissions = lens _emPermissions (\ s a -> s{_emPermissions = a})

-- | The ID of the environment for the environment member.
emEnvironmentId :: Lens' EnvironmentMember (Maybe Text)
emEnvironmentId = lens _emEnvironmentId (\ s a -> s{_emEnvironmentId = a})

instance FromJSON EnvironmentMember where
        parseJSON
          = withObject "EnvironmentMember"
              (\ x ->
                 EnvironmentMember' <$>
                   (x .:? "lastAccess") <*> (x .:? "userId") <*>
                     (x .:? "userArn")
                     <*> (x .:? "permissions")
                     <*> (x .:? "environmentId"))

instance Hashable EnvironmentMember where

instance NFData EnvironmentMember where
