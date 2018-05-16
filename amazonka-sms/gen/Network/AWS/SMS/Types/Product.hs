{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SMS.Types.Product where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SMS.Types.Sum

-- | Object representing a Connector
--
-- /See:/ 'connector' smart constructor.
data Connector = Connector'
  { _cStatus         :: !(Maybe ConnectorStatus)
  , _cVmManagerName  :: !(Maybe Text)
  , _cIpAddress      :: !(Maybe Text)
  , _cVmManagerId    :: !(Maybe Text)
  , _cVmManagerType  :: !(Maybe VMManagerType)
  , _cConnectorId    :: !(Maybe Text)
  , _cAssociatedOn   :: !(Maybe POSIX)
  , _cMacAddress     :: !(Maybe Text)
  , _cVersion        :: !(Maybe Text)
  , _cCapabilityList :: !(Maybe [ConnectorCapability])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Connector' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cStatus' - Undocumented member.
--
-- * 'cVmManagerName' - Undocumented member.
--
-- * 'cIpAddress' - Undocumented member.
--
-- * 'cVmManagerId' - Undocumented member.
--
-- * 'cVmManagerType' - Undocumented member.
--
-- * 'cConnectorId' - Undocumented member.
--
-- * 'cAssociatedOn' - Undocumented member.
--
-- * 'cMacAddress' - Undocumented member.
--
-- * 'cVersion' - Undocumented member.
--
-- * 'cCapabilityList' - Undocumented member.
connector
    :: Connector
connector =
  Connector'
    { _cStatus = Nothing
    , _cVmManagerName = Nothing
    , _cIpAddress = Nothing
    , _cVmManagerId = Nothing
    , _cVmManagerType = Nothing
    , _cConnectorId = Nothing
    , _cAssociatedOn = Nothing
    , _cMacAddress = Nothing
    , _cVersion = Nothing
    , _cCapabilityList = Nothing
    }


-- | Undocumented member.
cStatus :: Lens' Connector (Maybe ConnectorStatus)
cStatus = lens _cStatus (\ s a -> s{_cStatus = a})

-- | Undocumented member.
cVmManagerName :: Lens' Connector (Maybe Text)
cVmManagerName = lens _cVmManagerName (\ s a -> s{_cVmManagerName = a})

-- | Undocumented member.
cIpAddress :: Lens' Connector (Maybe Text)
cIpAddress = lens _cIpAddress (\ s a -> s{_cIpAddress = a})

-- | Undocumented member.
cVmManagerId :: Lens' Connector (Maybe Text)
cVmManagerId = lens _cVmManagerId (\ s a -> s{_cVmManagerId = a})

-- | Undocumented member.
cVmManagerType :: Lens' Connector (Maybe VMManagerType)
cVmManagerType = lens _cVmManagerType (\ s a -> s{_cVmManagerType = a})

-- | Undocumented member.
cConnectorId :: Lens' Connector (Maybe Text)
cConnectorId = lens _cConnectorId (\ s a -> s{_cConnectorId = a})

-- | Undocumented member.
cAssociatedOn :: Lens' Connector (Maybe UTCTime)
cAssociatedOn = lens _cAssociatedOn (\ s a -> s{_cAssociatedOn = a}) . mapping _Time

-- | Undocumented member.
cMacAddress :: Lens' Connector (Maybe Text)
cMacAddress = lens _cMacAddress (\ s a -> s{_cMacAddress = a})

-- | Undocumented member.
cVersion :: Lens' Connector (Maybe Text)
cVersion = lens _cVersion (\ s a -> s{_cVersion = a})

-- | Undocumented member.
cCapabilityList :: Lens' Connector [ConnectorCapability]
cCapabilityList = lens _cCapabilityList (\ s a -> s{_cCapabilityList = a}) . _Default . _Coerce

instance FromJSON Connector where
        parseJSON
          = withObject "Connector"
              (\ x ->
                 Connector' <$>
                   (x .:? "status") <*> (x .:? "vmManagerName") <*>
                     (x .:? "ipAddress")
                     <*> (x .:? "vmManagerId")
                     <*> (x .:? "vmManagerType")
                     <*> (x .:? "connectorId")
                     <*> (x .:? "associatedOn")
                     <*> (x .:? "macAddress")
                     <*> (x .:? "version")
                     <*> (x .:? "capabilityList" .!= mempty))

instance Hashable Connector where

instance NFData Connector where

-- | Object representing a Replication Job
--
-- /See:/ 'replicationJob' smart constructor.
data ReplicationJob = ReplicationJob'
  { _rjFrequency                   :: !(Maybe Int)
  , _rjState                       :: !(Maybe ReplicationJobState)
  , _rjServerType                  :: !(Maybe ServerType)
  , _rjServerId                    :: !(Maybe Text)
  , _rjLicenseType                 :: !(Maybe LicenseType)
  , _rjRoleName                    :: !(Maybe Text)
  , _rjVmServer                    :: !(Maybe VMServer)
  , _rjReplicationJobId            :: !(Maybe Text)
  , _rjReplicationRunList          :: !(Maybe [ReplicationRun])
  , _rjNextReplicationRunStartTime :: !(Maybe POSIX)
  , _rjStatusMessage               :: !(Maybe Text)
  , _rjLatestAMIId                 :: !(Maybe Text)
  , _rjSeedReplicationTime         :: !(Maybe POSIX)
  , _rjDescription                 :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjFrequency' - Undocumented member.
--
-- * 'rjState' - Undocumented member.
--
-- * 'rjServerType' - Undocumented member.
--
-- * 'rjServerId' - Undocumented member.
--
-- * 'rjLicenseType' - Undocumented member.
--
-- * 'rjRoleName' - Undocumented member.
--
-- * 'rjVmServer' - Undocumented member.
--
-- * 'rjReplicationJobId' - Undocumented member.
--
-- * 'rjReplicationRunList' - Undocumented member.
--
-- * 'rjNextReplicationRunStartTime' - Undocumented member.
--
-- * 'rjStatusMessage' - Undocumented member.
--
-- * 'rjLatestAMIId' - Undocumented member.
--
-- * 'rjSeedReplicationTime' - Undocumented member.
--
-- * 'rjDescription' - Undocumented member.
replicationJob
    :: ReplicationJob
replicationJob =
  ReplicationJob'
    { _rjFrequency = Nothing
    , _rjState = Nothing
    , _rjServerType = Nothing
    , _rjServerId = Nothing
    , _rjLicenseType = Nothing
    , _rjRoleName = Nothing
    , _rjVmServer = Nothing
    , _rjReplicationJobId = Nothing
    , _rjReplicationRunList = Nothing
    , _rjNextReplicationRunStartTime = Nothing
    , _rjStatusMessage = Nothing
    , _rjLatestAMIId = Nothing
    , _rjSeedReplicationTime = Nothing
    , _rjDescription = Nothing
    }


-- | Undocumented member.
rjFrequency :: Lens' ReplicationJob (Maybe Int)
rjFrequency = lens _rjFrequency (\ s a -> s{_rjFrequency = a})

-- | Undocumented member.
rjState :: Lens' ReplicationJob (Maybe ReplicationJobState)
rjState = lens _rjState (\ s a -> s{_rjState = a})

-- | Undocumented member.
rjServerType :: Lens' ReplicationJob (Maybe ServerType)
rjServerType = lens _rjServerType (\ s a -> s{_rjServerType = a})

-- | Undocumented member.
rjServerId :: Lens' ReplicationJob (Maybe Text)
rjServerId = lens _rjServerId (\ s a -> s{_rjServerId = a})

-- | Undocumented member.
rjLicenseType :: Lens' ReplicationJob (Maybe LicenseType)
rjLicenseType = lens _rjLicenseType (\ s a -> s{_rjLicenseType = a})

-- | Undocumented member.
rjRoleName :: Lens' ReplicationJob (Maybe Text)
rjRoleName = lens _rjRoleName (\ s a -> s{_rjRoleName = a})

-- | Undocumented member.
rjVmServer :: Lens' ReplicationJob (Maybe VMServer)
rjVmServer = lens _rjVmServer (\ s a -> s{_rjVmServer = a})

-- | Undocumented member.
rjReplicationJobId :: Lens' ReplicationJob (Maybe Text)
rjReplicationJobId = lens _rjReplicationJobId (\ s a -> s{_rjReplicationJobId = a})

-- | Undocumented member.
rjReplicationRunList :: Lens' ReplicationJob [ReplicationRun]
rjReplicationRunList = lens _rjReplicationRunList (\ s a -> s{_rjReplicationRunList = a}) . _Default . _Coerce

-- | Undocumented member.
rjNextReplicationRunStartTime :: Lens' ReplicationJob (Maybe UTCTime)
rjNextReplicationRunStartTime = lens _rjNextReplicationRunStartTime (\ s a -> s{_rjNextReplicationRunStartTime = a}) . mapping _Time

-- | Undocumented member.
rjStatusMessage :: Lens' ReplicationJob (Maybe Text)
rjStatusMessage = lens _rjStatusMessage (\ s a -> s{_rjStatusMessage = a})

-- | Undocumented member.
rjLatestAMIId :: Lens' ReplicationJob (Maybe Text)
rjLatestAMIId = lens _rjLatestAMIId (\ s a -> s{_rjLatestAMIId = a})

-- | Undocumented member.
rjSeedReplicationTime :: Lens' ReplicationJob (Maybe UTCTime)
rjSeedReplicationTime = lens _rjSeedReplicationTime (\ s a -> s{_rjSeedReplicationTime = a}) . mapping _Time

-- | Undocumented member.
rjDescription :: Lens' ReplicationJob (Maybe Text)
rjDescription = lens _rjDescription (\ s a -> s{_rjDescription = a})

instance FromJSON ReplicationJob where
        parseJSON
          = withObject "ReplicationJob"
              (\ x ->
                 ReplicationJob' <$>
                   (x .:? "frequency") <*> (x .:? "state") <*>
                     (x .:? "serverType")
                     <*> (x .:? "serverId")
                     <*> (x .:? "licenseType")
                     <*> (x .:? "roleName")
                     <*> (x .:? "vmServer")
                     <*> (x .:? "replicationJobId")
                     <*> (x .:? "replicationRunList" .!= mempty)
                     <*> (x .:? "nextReplicationRunStartTime")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "latestAmiId")
                     <*> (x .:? "seedReplicationTime")
                     <*> (x .:? "description"))

instance Hashable ReplicationJob where

instance NFData ReplicationJob where

-- | Object representing a Replication Run
--
-- /See:/ 'replicationRun' smart constructor.
data ReplicationRun = ReplicationRun'
  { _rrState              :: !(Maybe ReplicationRunState)
  , _rrReplicationRunId   :: !(Maybe Text)
  , _rrScheduledStartTime :: !(Maybe POSIX)
  , _rrStatusMessage      :: !(Maybe Text)
  , _rrCompletedTime      :: !(Maybe POSIX)
  , _rrAmiId              :: !(Maybe Text)
  , _rrType               :: !(Maybe ReplicationRunType)
  , _rrDescription        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReplicationRun' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rrState' - Undocumented member.
--
-- * 'rrReplicationRunId' - Undocumented member.
--
-- * 'rrScheduledStartTime' - Undocumented member.
--
-- * 'rrStatusMessage' - Undocumented member.
--
-- * 'rrCompletedTime' - Undocumented member.
--
-- * 'rrAmiId' - Undocumented member.
--
-- * 'rrType' - Undocumented member.
--
-- * 'rrDescription' - Undocumented member.
replicationRun
    :: ReplicationRun
replicationRun =
  ReplicationRun'
    { _rrState = Nothing
    , _rrReplicationRunId = Nothing
    , _rrScheduledStartTime = Nothing
    , _rrStatusMessage = Nothing
    , _rrCompletedTime = Nothing
    , _rrAmiId = Nothing
    , _rrType = Nothing
    , _rrDescription = Nothing
    }


-- | Undocumented member.
rrState :: Lens' ReplicationRun (Maybe ReplicationRunState)
rrState = lens _rrState (\ s a -> s{_rrState = a})

-- | Undocumented member.
rrReplicationRunId :: Lens' ReplicationRun (Maybe Text)
rrReplicationRunId = lens _rrReplicationRunId (\ s a -> s{_rrReplicationRunId = a})

-- | Undocumented member.
rrScheduledStartTime :: Lens' ReplicationRun (Maybe UTCTime)
rrScheduledStartTime = lens _rrScheduledStartTime (\ s a -> s{_rrScheduledStartTime = a}) . mapping _Time

-- | Undocumented member.
rrStatusMessage :: Lens' ReplicationRun (Maybe Text)
rrStatusMessage = lens _rrStatusMessage (\ s a -> s{_rrStatusMessage = a})

-- | Undocumented member.
rrCompletedTime :: Lens' ReplicationRun (Maybe UTCTime)
rrCompletedTime = lens _rrCompletedTime (\ s a -> s{_rrCompletedTime = a}) . mapping _Time

-- | Undocumented member.
rrAmiId :: Lens' ReplicationRun (Maybe Text)
rrAmiId = lens _rrAmiId (\ s a -> s{_rrAmiId = a})

-- | Undocumented member.
rrType :: Lens' ReplicationRun (Maybe ReplicationRunType)
rrType = lens _rrType (\ s a -> s{_rrType = a})

-- | Undocumented member.
rrDescription :: Lens' ReplicationRun (Maybe Text)
rrDescription = lens _rrDescription (\ s a -> s{_rrDescription = a})

instance FromJSON ReplicationRun where
        parseJSON
          = withObject "ReplicationRun"
              (\ x ->
                 ReplicationRun' <$>
                   (x .:? "state") <*> (x .:? "replicationRunId") <*>
                     (x .:? "scheduledStartTime")
                     <*> (x .:? "statusMessage")
                     <*> (x .:? "completedTime")
                     <*> (x .:? "amiId")
                     <*> (x .:? "type")
                     <*> (x .:? "description"))

instance Hashable ReplicationRun where

instance NFData ReplicationRun where

-- | Object representing a server
--
-- /See:/ 'server' smart constructor.
data Server = Server'
  { _sServerType               :: !(Maybe ServerType)
  , _sServerId                 :: !(Maybe Text)
  , _sReplicationJobTerminated :: !(Maybe Bool)
  , _sVmServer                 :: !(Maybe VMServer)
  , _sReplicationJobId         :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Server' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sServerType' - Undocumented member.
--
-- * 'sServerId' - Undocumented member.
--
-- * 'sReplicationJobTerminated' - Undocumented member.
--
-- * 'sVmServer' - Undocumented member.
--
-- * 'sReplicationJobId' - Undocumented member.
server
    :: Server
server =
  Server'
    { _sServerType = Nothing
    , _sServerId = Nothing
    , _sReplicationJobTerminated = Nothing
    , _sVmServer = Nothing
    , _sReplicationJobId = Nothing
    }


-- | Undocumented member.
sServerType :: Lens' Server (Maybe ServerType)
sServerType = lens _sServerType (\ s a -> s{_sServerType = a})

-- | Undocumented member.
sServerId :: Lens' Server (Maybe Text)
sServerId = lens _sServerId (\ s a -> s{_sServerId = a})

-- | Undocumented member.
sReplicationJobTerminated :: Lens' Server (Maybe Bool)
sReplicationJobTerminated = lens _sReplicationJobTerminated (\ s a -> s{_sReplicationJobTerminated = a})

-- | Undocumented member.
sVmServer :: Lens' Server (Maybe VMServer)
sVmServer = lens _sVmServer (\ s a -> s{_sVmServer = a})

-- | Undocumented member.
sReplicationJobId :: Lens' Server (Maybe Text)
sReplicationJobId = lens _sReplicationJobId (\ s a -> s{_sReplicationJobId = a})

instance FromJSON Server where
        parseJSON
          = withObject "Server"
              (\ x ->
                 Server' <$>
                   (x .:? "serverType") <*> (x .:? "serverId") <*>
                     (x .:? "replicationJobTerminated")
                     <*> (x .:? "vmServer")
                     <*> (x .:? "replicationJobId"))

instance Hashable Server where

instance NFData Server where

-- | Object representing a VM server
--
-- /See:/ 'vMServer' smart constructor.
data VMServer = VMServer'
  { _vmsVmManagerName   :: !(Maybe Text)
  , _vmsVmManagerType   :: !(Maybe VMManagerType)
  , _vmsVmServerAddress :: !(Maybe VMServerAddress)
  , _vmsVmName          :: !(Maybe Text)
  , _vmsVmPath          :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VMServer' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmsVmManagerName' - Undocumented member.
--
-- * 'vmsVmManagerType' - Undocumented member.
--
-- * 'vmsVmServerAddress' - Undocumented member.
--
-- * 'vmsVmName' - Undocumented member.
--
-- * 'vmsVmPath' - Undocumented member.
vMServer
    :: VMServer
vMServer =
  VMServer'
    { _vmsVmManagerName = Nothing
    , _vmsVmManagerType = Nothing
    , _vmsVmServerAddress = Nothing
    , _vmsVmName = Nothing
    , _vmsVmPath = Nothing
    }


-- | Undocumented member.
vmsVmManagerName :: Lens' VMServer (Maybe Text)
vmsVmManagerName = lens _vmsVmManagerName (\ s a -> s{_vmsVmManagerName = a})

-- | Undocumented member.
vmsVmManagerType :: Lens' VMServer (Maybe VMManagerType)
vmsVmManagerType = lens _vmsVmManagerType (\ s a -> s{_vmsVmManagerType = a})

-- | Undocumented member.
vmsVmServerAddress :: Lens' VMServer (Maybe VMServerAddress)
vmsVmServerAddress = lens _vmsVmServerAddress (\ s a -> s{_vmsVmServerAddress = a})

-- | Undocumented member.
vmsVmName :: Lens' VMServer (Maybe Text)
vmsVmName = lens _vmsVmName (\ s a -> s{_vmsVmName = a})

-- | Undocumented member.
vmsVmPath :: Lens' VMServer (Maybe Text)
vmsVmPath = lens _vmsVmPath (\ s a -> s{_vmsVmPath = a})

instance FromJSON VMServer where
        parseJSON
          = withObject "VMServer"
              (\ x ->
                 VMServer' <$>
                   (x .:? "vmManagerName") <*> (x .:? "vmManagerType")
                     <*> (x .:? "vmServerAddress")
                     <*> (x .:? "vmName")
                     <*> (x .:? "vmPath"))

instance Hashable VMServer where

instance NFData VMServer where

-- | Object representing a server's location
--
-- /See:/ 'vMServerAddress' smart constructor.
data VMServerAddress = VMServerAddress'
  { _vmsaVmManagerId :: !(Maybe Text)
  , _vmsaVmId        :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'VMServerAddress' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmsaVmManagerId' - Undocumented member.
--
-- * 'vmsaVmId' - Undocumented member.
vMServerAddress
    :: VMServerAddress
vMServerAddress =
  VMServerAddress' {_vmsaVmManagerId = Nothing, _vmsaVmId = Nothing}


-- | Undocumented member.
vmsaVmManagerId :: Lens' VMServerAddress (Maybe Text)
vmsaVmManagerId = lens _vmsaVmManagerId (\ s a -> s{_vmsaVmManagerId = a})

-- | Undocumented member.
vmsaVmId :: Lens' VMServerAddress (Maybe Text)
vmsaVmId = lens _vmsaVmId (\ s a -> s{_vmsaVmId = a})

instance FromJSON VMServerAddress where
        parseJSON
          = withObject "VMServerAddress"
              (\ x ->
                 VMServerAddress' <$>
                   (x .:? "vmManagerId") <*> (x .:? "vmId"))

instance Hashable VMServerAddress where

instance NFData VMServerAddress where
