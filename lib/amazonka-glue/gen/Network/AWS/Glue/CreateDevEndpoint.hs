{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.CreateDevEndpoint
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new DevEndpoint.
--
--
module Network.AWS.Glue.CreateDevEndpoint
    (
    -- * Creating a Request
      createDevEndpoint
    , CreateDevEndpoint
    -- * Request Lenses
    , cdeExtraPythonLibsS3Path
    , cdeSecurityGroupIds
    , cdePublicKey
    , cdeSubnetId
    , cdeNumberOfNodes
    , cdeExtraJARsS3Path
    , cdeEndpointName
    , cdeRoleARN

    -- * Destructuring the Response
    , createDevEndpointResponse
    , CreateDevEndpointResponse
    -- * Response Lenses
    , cdersStatus
    , cdersFailureReason
    , cdersEndpointName
    , cdersExtraPythonLibsS3Path
    , cdersSecurityGroupIds
    , cdersVPCId
    , cdersSubnetId
    , cdersNumberOfNodes
    , cdersAvailabilityZone
    , cdersZeppelinRemoteSparkInterpreterPort
    , cdersExtraJARsS3Path
    , cdersCreatedTimestamp
    , cdersYarnEndpointAddress
    , cdersRoleARN
    , cdersResponseStatus
    ) where

import Network.AWS.Glue.Types
import Network.AWS.Glue.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDevEndpoint' smart constructor.
data CreateDevEndpoint = CreateDevEndpoint'
  { _cdeExtraPythonLibsS3Path :: !(Maybe Text)
  , _cdeSecurityGroupIds      :: !(Maybe [Text])
  , _cdePublicKey             :: !(Maybe Text)
  , _cdeSubnetId              :: !(Maybe Text)
  , _cdeNumberOfNodes         :: !(Maybe Int)
  , _cdeExtraJARsS3Path       :: !(Maybe Text)
  , _cdeEndpointName          :: !Text
  , _cdeRoleARN               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDevEndpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdeExtraPythonLibsS3Path' - Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
--
-- * 'cdeSecurityGroupIds' - Security group IDs for the security groups to be used by the new DevEndpoint.
--
-- * 'cdePublicKey' - The public key to use for authentication.
--
-- * 'cdeSubnetId' - The subnet ID for the new DevEndpoint to use.
--
-- * 'cdeNumberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) to allocate to this DevEndpoint.
--
-- * 'cdeExtraJARsS3Path' - Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint.
--
-- * 'cdeEndpointName' - The name to be assigned to the new DevEndpoint.
--
-- * 'cdeRoleARN' - The IAM role for the DevEndpoint.
createDevEndpoint
    :: Text -- ^ 'cdeEndpointName'
    -> Text -- ^ 'cdeRoleARN'
    -> CreateDevEndpoint
createDevEndpoint pEndpointName_ pRoleARN_ =
  CreateDevEndpoint'
    { _cdeExtraPythonLibsS3Path = Nothing
    , _cdeSecurityGroupIds = Nothing
    , _cdePublicKey = Nothing
    , _cdeSubnetId = Nothing
    , _cdeNumberOfNodes = Nothing
    , _cdeExtraJARsS3Path = Nothing
    , _cdeEndpointName = pEndpointName_
    , _cdeRoleARN = pRoleARN_
    }


-- | Path(s) to one or more Python libraries in an S3 bucket that should be loaded in your DevEndpoint. Multiple values must be complete paths separated by a comma. Please note that only pure Python libraries can currently be used on a DevEndpoint. Libraries that rely on C extensions, such as the <http://pandas.pydata.org/ pandas> Python data analysis library, are not yet supported.
cdeExtraPythonLibsS3Path :: Lens' CreateDevEndpoint (Maybe Text)
cdeExtraPythonLibsS3Path = lens _cdeExtraPythonLibsS3Path (\ s a -> s{_cdeExtraPythonLibsS3Path = a})

-- | Security group IDs for the security groups to be used by the new DevEndpoint.
cdeSecurityGroupIds :: Lens' CreateDevEndpoint [Text]
cdeSecurityGroupIds = lens _cdeSecurityGroupIds (\ s a -> s{_cdeSecurityGroupIds = a}) . _Default . _Coerce

-- | The public key to use for authentication.
cdePublicKey :: Lens' CreateDevEndpoint (Maybe Text)
cdePublicKey = lens _cdePublicKey (\ s a -> s{_cdePublicKey = a})

-- | The subnet ID for the new DevEndpoint to use.
cdeSubnetId :: Lens' CreateDevEndpoint (Maybe Text)
cdeSubnetId = lens _cdeSubnetId (\ s a -> s{_cdeSubnetId = a})

-- | The number of AWS Glue Data Processing Units (DPUs) to allocate to this DevEndpoint.
cdeNumberOfNodes :: Lens' CreateDevEndpoint (Maybe Int)
cdeNumberOfNodes = lens _cdeNumberOfNodes (\ s a -> s{_cdeNumberOfNodes = a})

-- | Path to one or more Java Jars in an S3 bucket that should be loaded in your DevEndpoint.
cdeExtraJARsS3Path :: Lens' CreateDevEndpoint (Maybe Text)
cdeExtraJARsS3Path = lens _cdeExtraJARsS3Path (\ s a -> s{_cdeExtraJARsS3Path = a})

-- | The name to be assigned to the new DevEndpoint.
cdeEndpointName :: Lens' CreateDevEndpoint Text
cdeEndpointName = lens _cdeEndpointName (\ s a -> s{_cdeEndpointName = a})

-- | The IAM role for the DevEndpoint.
cdeRoleARN :: Lens' CreateDevEndpoint Text
cdeRoleARN = lens _cdeRoleARN (\ s a -> s{_cdeRoleARN = a})

instance AWSRequest CreateDevEndpoint where
        type Rs CreateDevEndpoint = CreateDevEndpointResponse
        request = postJSON glue
        response
          = receiveJSON
              (\ s h x ->
                 CreateDevEndpointResponse' <$>
                   (x .?> "Status") <*> (x .?> "FailureReason") <*>
                     (x .?> "EndpointName")
                     <*> (x .?> "ExtraPythonLibsS3Path")
                     <*> (x .?> "SecurityGroupIds" .!@ mempty)
                     <*> (x .?> "VpcId")
                     <*> (x .?> "SubnetId")
                     <*> (x .?> "NumberOfNodes")
                     <*> (x .?> "AvailabilityZone")
                     <*> (x .?> "ZeppelinRemoteSparkInterpreterPort")
                     <*> (x .?> "ExtraJarsS3Path")
                     <*> (x .?> "CreatedTimestamp")
                     <*> (x .?> "YarnEndpointAddress")
                     <*> (x .?> "RoleArn")
                     <*> (pure (fromEnum s)))

instance Hashable CreateDevEndpoint where

instance NFData CreateDevEndpoint where

instance ToHeaders CreateDevEndpoint where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSGlue.CreateDevEndpoint" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDevEndpoint where
        toJSON CreateDevEndpoint'{..}
          = object
              (catMaybes
                 [("ExtraPythonLibsS3Path" .=) <$>
                    _cdeExtraPythonLibsS3Path,
                  ("SecurityGroupIds" .=) <$> _cdeSecurityGroupIds,
                  ("PublicKey" .=) <$> _cdePublicKey,
                  ("SubnetId" .=) <$> _cdeSubnetId,
                  ("NumberOfNodes" .=) <$> _cdeNumberOfNodes,
                  ("ExtraJarsS3Path" .=) <$> _cdeExtraJARsS3Path,
                  Just ("EndpointName" .= _cdeEndpointName),
                  Just ("RoleArn" .= _cdeRoleARN)])

instance ToPath CreateDevEndpoint where
        toPath = const "/"

instance ToQuery CreateDevEndpoint where
        toQuery = const mempty

-- | /See:/ 'createDevEndpointResponse' smart constructor.
data CreateDevEndpointResponse = CreateDevEndpointResponse'
  { _cdersStatus                             :: !(Maybe Text)
  , _cdersFailureReason                      :: !(Maybe Text)
  , _cdersEndpointName                       :: !(Maybe Text)
  , _cdersExtraPythonLibsS3Path              :: !(Maybe Text)
  , _cdersSecurityGroupIds                   :: !(Maybe [Text])
  , _cdersVPCId                              :: !(Maybe Text)
  , _cdersSubnetId                           :: !(Maybe Text)
  , _cdersNumberOfNodes                      :: !(Maybe Int)
  , _cdersAvailabilityZone                   :: !(Maybe Text)
  , _cdersZeppelinRemoteSparkInterpreterPort :: !(Maybe Int)
  , _cdersExtraJARsS3Path                    :: !(Maybe Text)
  , _cdersCreatedTimestamp                   :: !(Maybe POSIX)
  , _cdersYarnEndpointAddress                :: !(Maybe Text)
  , _cdersRoleARN                            :: !(Maybe Text)
  , _cdersResponseStatus                     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDevEndpointResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdersStatus' - The current status of the new DevEndpoint.
--
-- * 'cdersFailureReason' - The reason for a current failure in this DevEndpoint.
--
-- * 'cdersEndpointName' - The name assigned to the new DevEndpoint.
--
-- * 'cdersExtraPythonLibsS3Path' - Path(s) to one or more Python libraries in an S3 bucket that will be loaded in your DevEndpoint.
--
-- * 'cdersSecurityGroupIds' - The security groups assigned to the new DevEndpoint.
--
-- * 'cdersVPCId' - The ID of the VPC used by this DevEndpoint.
--
-- * 'cdersSubnetId' - The subnet ID assigned to the new DevEndpoint.
--
-- * 'cdersNumberOfNodes' - The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
--
-- * 'cdersAvailabilityZone' - The AWS availability zone where this DevEndpoint is located.
--
-- * 'cdersZeppelinRemoteSparkInterpreterPort' - The Apache Zeppelin port for the remote Apache Spark interpreter.
--
-- * 'cdersExtraJARsS3Path' - Path to one or more Java Jars in an S3 bucket that will be loaded in your DevEndpoint.
--
-- * 'cdersCreatedTimestamp' - The point in time at which this DevEndpoint was created.
--
-- * 'cdersYarnEndpointAddress' - The address of the YARN endpoint used by this DevEndpoint.
--
-- * 'cdersRoleARN' - The AWS ARN of the role assigned to the new DevEndpoint.
--
-- * 'cdersResponseStatus' - -- | The response status code.
createDevEndpointResponse
    :: Int -- ^ 'cdersResponseStatus'
    -> CreateDevEndpointResponse
createDevEndpointResponse pResponseStatus_ =
  CreateDevEndpointResponse'
    { _cdersStatus = Nothing
    , _cdersFailureReason = Nothing
    , _cdersEndpointName = Nothing
    , _cdersExtraPythonLibsS3Path = Nothing
    , _cdersSecurityGroupIds = Nothing
    , _cdersVPCId = Nothing
    , _cdersSubnetId = Nothing
    , _cdersNumberOfNodes = Nothing
    , _cdersAvailabilityZone = Nothing
    , _cdersZeppelinRemoteSparkInterpreterPort = Nothing
    , _cdersExtraJARsS3Path = Nothing
    , _cdersCreatedTimestamp = Nothing
    , _cdersYarnEndpointAddress = Nothing
    , _cdersRoleARN = Nothing
    , _cdersResponseStatus = pResponseStatus_
    }


-- | The current status of the new DevEndpoint.
cdersStatus :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersStatus = lens _cdersStatus (\ s a -> s{_cdersStatus = a})

-- | The reason for a current failure in this DevEndpoint.
cdersFailureReason :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersFailureReason = lens _cdersFailureReason (\ s a -> s{_cdersFailureReason = a})

-- | The name assigned to the new DevEndpoint.
cdersEndpointName :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersEndpointName = lens _cdersEndpointName (\ s a -> s{_cdersEndpointName = a})

-- | Path(s) to one or more Python libraries in an S3 bucket that will be loaded in your DevEndpoint.
cdersExtraPythonLibsS3Path :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersExtraPythonLibsS3Path = lens _cdersExtraPythonLibsS3Path (\ s a -> s{_cdersExtraPythonLibsS3Path = a})

-- | The security groups assigned to the new DevEndpoint.
cdersSecurityGroupIds :: Lens' CreateDevEndpointResponse [Text]
cdersSecurityGroupIds = lens _cdersSecurityGroupIds (\ s a -> s{_cdersSecurityGroupIds = a}) . _Default . _Coerce

-- | The ID of the VPC used by this DevEndpoint.
cdersVPCId :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersVPCId = lens _cdersVPCId (\ s a -> s{_cdersVPCId = a})

-- | The subnet ID assigned to the new DevEndpoint.
cdersSubnetId :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersSubnetId = lens _cdersSubnetId (\ s a -> s{_cdersSubnetId = a})

-- | The number of AWS Glue Data Processing Units (DPUs) allocated to this DevEndpoint.
cdersNumberOfNodes :: Lens' CreateDevEndpointResponse (Maybe Int)
cdersNumberOfNodes = lens _cdersNumberOfNodes (\ s a -> s{_cdersNumberOfNodes = a})

-- | The AWS availability zone where this DevEndpoint is located.
cdersAvailabilityZone :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersAvailabilityZone = lens _cdersAvailabilityZone (\ s a -> s{_cdersAvailabilityZone = a})

-- | The Apache Zeppelin port for the remote Apache Spark interpreter.
cdersZeppelinRemoteSparkInterpreterPort :: Lens' CreateDevEndpointResponse (Maybe Int)
cdersZeppelinRemoteSparkInterpreterPort = lens _cdersZeppelinRemoteSparkInterpreterPort (\ s a -> s{_cdersZeppelinRemoteSparkInterpreterPort = a})

-- | Path to one or more Java Jars in an S3 bucket that will be loaded in your DevEndpoint.
cdersExtraJARsS3Path :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersExtraJARsS3Path = lens _cdersExtraJARsS3Path (\ s a -> s{_cdersExtraJARsS3Path = a})

-- | The point in time at which this DevEndpoint was created.
cdersCreatedTimestamp :: Lens' CreateDevEndpointResponse (Maybe UTCTime)
cdersCreatedTimestamp = lens _cdersCreatedTimestamp (\ s a -> s{_cdersCreatedTimestamp = a}) . mapping _Time

-- | The address of the YARN endpoint used by this DevEndpoint.
cdersYarnEndpointAddress :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersYarnEndpointAddress = lens _cdersYarnEndpointAddress (\ s a -> s{_cdersYarnEndpointAddress = a})

-- | The AWS ARN of the role assigned to the new DevEndpoint.
cdersRoleARN :: Lens' CreateDevEndpointResponse (Maybe Text)
cdersRoleARN = lens _cdersRoleARN (\ s a -> s{_cdersRoleARN = a})

-- | -- | The response status code.
cdersResponseStatus :: Lens' CreateDevEndpointResponse Int
cdersResponseStatus = lens _cdersResponseStatus (\ s a -> s{_cdersResponseStatus = a})

instance NFData CreateDevEndpointResponse where
