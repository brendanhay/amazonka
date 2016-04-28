{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.Product
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.Product where

import           Network.AWS.Lambda.Types.Sum
import           Network.AWS.Lens
import           Network.AWS.Prelude

-- | Provides configuration information about a Lambda function version
-- alias.
--
-- /See:/ 'aliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
    { _acName            :: !(Maybe Text)
    , _acFunctionVersion :: !(Maybe Text)
    , _acAliasARN        :: !(Maybe Text)
    , _acDescription     :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AliasConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acName'
--
-- * 'acFunctionVersion'
--
-- * 'acAliasARN'
--
-- * 'acDescription'
aliasConfiguration
    :: AliasConfiguration
aliasConfiguration =
    AliasConfiguration'
    { _acName = Nothing
    , _acFunctionVersion = Nothing
    , _acAliasARN = Nothing
    , _acDescription = Nothing
    }

-- | Alias name.
acName :: Lens' AliasConfiguration (Maybe Text)
acName = lens _acName (\ s a -> s{_acName = a});

-- | Function version to which the alias points.
acFunctionVersion :: Lens' AliasConfiguration (Maybe Text)
acFunctionVersion = lens _acFunctionVersion (\ s a -> s{_acFunctionVersion = a});

-- | Lambda function ARN that is qualified using the alias name as the
-- suffix. For example, if you create an alias called 'BETA' that points to
-- a helloworld function version, the ARN is
-- 'arn:aws:lambda:aws-regions:acct-id:function:helloworld:BETA'.
acAliasARN :: Lens' AliasConfiguration (Maybe Text)
acAliasARN = lens _acAliasARN (\ s a -> s{_acAliasARN = a});

-- | Alias description.
acDescription :: Lens' AliasConfiguration (Maybe Text)
acDescription = lens _acDescription (\ s a -> s{_acDescription = a});

instance FromJSON AliasConfiguration where
        parseJSON
          = withObject "AliasConfiguration"
              (\ x ->
                 AliasConfiguration' <$>
                   (x .:? "Name") <*> (x .:? "FunctionVersion") <*>
                     (x .:? "AliasArn")
                     <*> (x .:? "Description"))

instance Hashable AliasConfiguration

instance NFData AliasConfiguration

-- | Describes mapping between an Amazon Kinesis stream and a Lambda
-- function.
--
-- /See:/ 'eventSourceMappingConfiguration' smart constructor.
data EventSourceMappingConfiguration = EventSourceMappingConfiguration'
    { _esmcEventSourceARN        :: !(Maybe Text)
    , _esmcState                 :: !(Maybe Text)
    , _esmcFunctionARN           :: !(Maybe Text)
    , _esmcUUId                  :: !(Maybe Text)
    , _esmcLastProcessingResult  :: !(Maybe Text)
    , _esmcBatchSize             :: !(Maybe Nat)
    , _esmcStateTransitionReason :: !(Maybe Text)
    , _esmcLastModified          :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'EventSourceMappingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esmcEventSourceARN'
--
-- * 'esmcState'
--
-- * 'esmcFunctionARN'
--
-- * 'esmcUUId'
--
-- * 'esmcLastProcessingResult'
--
-- * 'esmcBatchSize'
--
-- * 'esmcStateTransitionReason'
--
-- * 'esmcLastModified'
eventSourceMappingConfiguration
    :: EventSourceMappingConfiguration
eventSourceMappingConfiguration =
    EventSourceMappingConfiguration'
    { _esmcEventSourceARN = Nothing
    , _esmcState = Nothing
    , _esmcFunctionARN = Nothing
    , _esmcUUId = Nothing
    , _esmcLastProcessingResult = Nothing
    , _esmcBatchSize = Nothing
    , _esmcStateTransitionReason = Nothing
    , _esmcLastModified = Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis stream that is the
-- source of events.
esmcEventSourceARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcEventSourceARN = lens _esmcEventSourceARN (\ s a -> s{_esmcEventSourceARN = a});

-- | The state of the event source mapping. It can be 'Creating', 'Enabled',
-- 'Disabled', 'Enabling', 'Disabling', 'Updating', or 'Deleting'.
esmcState :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcState = lens _esmcState (\ s a -> s{_esmcState = a});

-- | The Lambda function to invoke when AWS Lambda detects an event on the
-- stream.
esmcFunctionARN :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcFunctionARN = lens _esmcFunctionARN (\ s a -> s{_esmcFunctionARN = a});

-- | The AWS Lambda assigned opaque identifier for the mapping.
esmcUUId :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcUUId = lens _esmcUUId (\ s a -> s{_esmcUUId = a});

-- | The result of the last AWS Lambda invocation of your Lambda function.
esmcLastProcessingResult :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcLastProcessingResult = lens _esmcLastProcessingResult (\ s a -> s{_esmcLastProcessingResult = a});

-- | The largest number of records that AWS Lambda will retrieve from your
-- event source at the time of invoking your function. Your function
-- receives an event with all the retrieved records.
esmcBatchSize :: Lens' EventSourceMappingConfiguration (Maybe Natural)
esmcBatchSize = lens _esmcBatchSize (\ s a -> s{_esmcBatchSize = a}) . mapping _Nat;

-- | The reason the event source mapping is in its current state. It is
-- either user-requested or an AWS Lambda-initiated state transition.
esmcStateTransitionReason :: Lens' EventSourceMappingConfiguration (Maybe Text)
esmcStateTransitionReason = lens _esmcStateTransitionReason (\ s a -> s{_esmcStateTransitionReason = a});

-- | The UTC time string indicating the last time the event mapping was
-- updated.
esmcLastModified :: Lens' EventSourceMappingConfiguration (Maybe UTCTime)
esmcLastModified = lens _esmcLastModified (\ s a -> s{_esmcLastModified = a}) . mapping _Time;

instance FromJSON EventSourceMappingConfiguration
         where
        parseJSON
          = withObject "EventSourceMappingConfiguration"
              (\ x ->
                 EventSourceMappingConfiguration' <$>
                   (x .:? "EventSourceArn") <*> (x .:? "State") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "UUID")
                     <*> (x .:? "LastProcessingResult")
                     <*> (x .:? "BatchSize")
                     <*> (x .:? "StateTransitionReason")
                     <*> (x .:? "LastModified"))

instance Hashable EventSourceMappingConfiguration

instance NFData EventSourceMappingConfiguration

-- | The code for the Lambda function.
--
-- /See:/ 'functionCode' smart constructor.
data FunctionCode = FunctionCode'
    { _fcS3ObjectVersion :: !(Maybe Text)
    , _fcS3Key           :: !(Maybe Text)
    , _fcZipFile         :: !(Maybe Base64)
    , _fcS3Bucket        :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionCode' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcS3ObjectVersion'
--
-- * 'fcS3Key'
--
-- * 'fcZipFile'
--
-- * 'fcS3Bucket'
functionCode
    :: FunctionCode
functionCode =
    FunctionCode'
    { _fcS3ObjectVersion = Nothing
    , _fcS3Key = Nothing
    , _fcZipFile = Nothing
    , _fcS3Bucket = Nothing
    }

-- | The Amazon S3 object (the deployment package) version you want to
-- upload.
fcS3ObjectVersion :: Lens' FunctionCode (Maybe Text)
fcS3ObjectVersion = lens _fcS3ObjectVersion (\ s a -> s{_fcS3ObjectVersion = a});

-- | The Amazon S3 object (the deployment package) key name you want to
-- upload.
fcS3Key :: Lens' FunctionCode (Maybe Text)
fcS3Key = lens _fcS3Key (\ s a -> s{_fcS3Key = a});

-- | A zip file containing your deployment package. If you are using the API
-- directly, the zip file must be base64-encoded (if you are using the AWS
-- SDKs or the AWS CLI, the SDKs or CLI will do the encoding for you). For
-- more information about creating a .zip file, go to
-- <http://docs.aws.amazon.com/lambda/latest/dg/intro-permission-model.html#lambda-intro-execution-role.html Execution Permissions>
-- in the /AWS Lambda Developer Guide/.
--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data,
-- despite what the AWS documentation might say.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
fcZipFile :: Lens' FunctionCode (Maybe ByteString)
fcZipFile = lens _fcZipFile (\ s a -> s{_fcZipFile = a}) . mapping _Base64;

-- | Amazon S3 bucket name where the .zip file containing your deployment
-- package is stored. This bucket must reside in the same AWS region where
-- you are creating the Lambda function.
fcS3Bucket :: Lens' FunctionCode (Maybe Text)
fcS3Bucket = lens _fcS3Bucket (\ s a -> s{_fcS3Bucket = a});

instance Hashable FunctionCode

instance NFData FunctionCode

instance ToJSON FunctionCode where
        toJSON FunctionCode'{..}
          = object
              (catMaybes
                 [("S3ObjectVersion" .=) <$> _fcS3ObjectVersion,
                  ("S3Key" .=) <$> _fcS3Key,
                  ("ZipFile" .=) <$> _fcZipFile,
                  ("S3Bucket" .=) <$> _fcS3Bucket])

-- | The object for the Lambda function location.
--
-- /See:/ 'functionCodeLocation' smart constructor.
data FunctionCodeLocation = FunctionCodeLocation'
    { _fclLocation       :: !(Maybe Text)
    , _fclRepositoryType :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionCodeLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fclLocation'
--
-- * 'fclRepositoryType'
functionCodeLocation
    :: FunctionCodeLocation
functionCodeLocation =
    FunctionCodeLocation'
    { _fclLocation = Nothing
    , _fclRepositoryType = Nothing
    }

-- | The presigned URL you can use to download the function\'s .zip file that
-- you previously uploaded. The URL is valid for up to 10 minutes.
fclLocation :: Lens' FunctionCodeLocation (Maybe Text)
fclLocation = lens _fclLocation (\ s a -> s{_fclLocation = a});

-- | The repository from which you can download the function.
fclRepositoryType :: Lens' FunctionCodeLocation (Maybe Text)
fclRepositoryType = lens _fclRepositoryType (\ s a -> s{_fclRepositoryType = a});

instance FromJSON FunctionCodeLocation where
        parseJSON
          = withObject "FunctionCodeLocation"
              (\ x ->
                 FunctionCodeLocation' <$>
                   (x .:? "Location") <*> (x .:? "RepositoryType"))

instance Hashable FunctionCodeLocation

instance NFData FunctionCodeLocation

-- | A complex type that describes function metadata.
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
    { _fcMemorySize   :: !(Maybe Nat)
    , _fcRuntime      :: !(Maybe Runtime)
    , _fcFunctionARN  :: !(Maybe Text)
    , _fcRole         :: !(Maybe Text)
    , _fcVPCConfig    :: !(Maybe VPCConfigResponse)
    , _fcVersion      :: !(Maybe Text)
    , _fcFunctionName :: !(Maybe Text)
    , _fcCodeSize     :: !(Maybe Integer)
    , _fcHandler      :: !(Maybe Text)
    , _fcTimeout      :: !(Maybe Nat)
    , _fcLastModified :: !(Maybe Text)
    , _fcCodeSha256   :: !(Maybe Text)
    , _fcDescription  :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcMemorySize'
--
-- * 'fcRuntime'
--
-- * 'fcFunctionARN'
--
-- * 'fcRole'
--
-- * 'fcVPCConfig'
--
-- * 'fcVersion'
--
-- * 'fcFunctionName'
--
-- * 'fcCodeSize'
--
-- * 'fcHandler'
--
-- * 'fcTimeout'
--
-- * 'fcLastModified'
--
-- * 'fcCodeSha256'
--
-- * 'fcDescription'
functionConfiguration
    :: FunctionConfiguration
functionConfiguration =
    FunctionConfiguration'
    { _fcMemorySize = Nothing
    , _fcRuntime = Nothing
    , _fcFunctionARN = Nothing
    , _fcRole = Nothing
    , _fcVPCConfig = Nothing
    , _fcVersion = Nothing
    , _fcFunctionName = Nothing
    , _fcCodeSize = Nothing
    , _fcHandler = Nothing
    , _fcTimeout = Nothing
    , _fcLastModified = Nothing
    , _fcCodeSha256 = Nothing
    , _fcDescription = Nothing
    }

-- | The memory size, in MB, you configured for the function. Must be a
-- multiple of 64 MB.
fcMemorySize :: Lens' FunctionConfiguration (Maybe Natural)
fcMemorySize = lens _fcMemorySize (\ s a -> s{_fcMemorySize = a}) . mapping _Nat;

-- | The runtime environment for the Lambda function.
fcRuntime :: Lens' FunctionConfiguration (Maybe Runtime)
fcRuntime = lens _fcRuntime (\ s a -> s{_fcRuntime = a});

-- | The Amazon Resource Name (ARN) assigned to the function.
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\ s a -> s{_fcFunctionARN = a});

-- | The Amazon Resource Name (ARN) of the IAM role that Lambda assumes when
-- it executes your function to access any other Amazon Web Services (AWS)
-- resources.
fcRole :: Lens' FunctionConfiguration (Maybe Text)
fcRole = lens _fcRole (\ s a -> s{_fcRole = a});

-- | VPC configuration associated with your Lambda function.
fcVPCConfig :: Lens' FunctionConfiguration (Maybe VPCConfigResponse)
fcVPCConfig = lens _fcVPCConfig (\ s a -> s{_fcVPCConfig = a});

-- | The version of the Lambda function.
fcVersion :: Lens' FunctionConfiguration (Maybe Text)
fcVersion = lens _fcVersion (\ s a -> s{_fcVersion = a});

-- | The name of the function.
fcFunctionName :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionName = lens _fcFunctionName (\ s a -> s{_fcFunctionName = a});

-- | The size, in bytes, of the function .zip file you uploaded.
fcCodeSize :: Lens' FunctionConfiguration (Maybe Integer)
fcCodeSize = lens _fcCodeSize (\ s a -> s{_fcCodeSize = a});

-- | The function Lambda calls to begin executing your function.
fcHandler :: Lens' FunctionConfiguration (Maybe Text)
fcHandler = lens _fcHandler (\ s a -> s{_fcHandler = a});

-- | The function execution time at which Lambda should terminate the
-- function. Because the execution time has cost implications, we recommend
-- you set this value based on your expected execution time. The default is
-- 3 seconds.
fcTimeout :: Lens' FunctionConfiguration (Maybe Natural)
fcTimeout = lens _fcTimeout (\ s a -> s{_fcTimeout = a}) . mapping _Nat;

-- | The time stamp of the last time you updated the function.
fcLastModified :: Lens' FunctionConfiguration (Maybe Text)
fcLastModified = lens _fcLastModified (\ s a -> s{_fcLastModified = a});

-- | It is the SHA256 hash of your function deployment package.
fcCodeSha256 :: Lens' FunctionConfiguration (Maybe Text)
fcCodeSha256 = lens _fcCodeSha256 (\ s a -> s{_fcCodeSha256 = a});

-- | The user-provided description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\ s a -> s{_fcDescription = a});

instance FromJSON FunctionConfiguration where
        parseJSON
          = withObject "FunctionConfiguration"
              (\ x ->
                 FunctionConfiguration' <$>
                   (x .:? "MemorySize") <*> (x .:? "Runtime") <*>
                     (x .:? "FunctionArn")
                     <*> (x .:? "Role")
                     <*> (x .:? "VpcConfig")
                     <*> (x .:? "Version")
                     <*> (x .:? "FunctionName")
                     <*> (x .:? "CodeSize")
                     <*> (x .:? "Handler")
                     <*> (x .:? "Timeout")
                     <*> (x .:? "LastModified")
                     <*> (x .:? "CodeSha256")
                     <*> (x .:? "Description"))

instance Hashable FunctionConfiguration

instance NFData FunctionConfiguration

-- | If your Lambda function accesses resources in a VPC, you provide this
-- parameter identifying the list of security group IDs and subnet IDs.
-- These must belong to the same VPC. You must provide at least one
-- security group and one subnet ID.
--
-- /See:/ 'vpcConfig' smart constructor.
data VPCConfig = VPCConfig'
    { _vpccSecurityGroupIds :: !(Maybe [Text])
    , _vpccSubnetIds        :: !(Maybe [Text])
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPCConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vpccSecurityGroupIds'
--
-- * 'vpccSubnetIds'
vpcConfig
    :: VPCConfig
vpcConfig =
    VPCConfig'
    { _vpccSecurityGroupIds = Nothing
    , _vpccSubnetIds = Nothing
    }

-- | A list of one or more security groups IDs in your VPC.
vpccSecurityGroupIds :: Lens' VPCConfig [Text]
vpccSecurityGroupIds = lens _vpccSecurityGroupIds (\ s a -> s{_vpccSecurityGroupIds = a}) . _Default . _Coerce;

-- | A list of one or more subnet IDs in your VPC.
vpccSubnetIds :: Lens' VPCConfig [Text]
vpccSubnetIds = lens _vpccSubnetIds (\ s a -> s{_vpccSubnetIds = a}) . _Default . _Coerce;

instance Hashable VPCConfig

instance NFData VPCConfig

instance ToJSON VPCConfig where
        toJSON VPCConfig'{..}
          = object
              (catMaybes
                 [("SecurityGroupIds" .=) <$> _vpccSecurityGroupIds,
                  ("SubnetIds" .=) <$> _vpccSubnetIds])

-- | VPC configuration associated with your Lambda function.
--
-- /See:/ 'vpcConfigResponse' smart constructor.
data VPCConfigResponse = VPCConfigResponse'
    { _vcSecurityGroupIds :: !(Maybe [Text])
    , _vcSubnetIds        :: !(Maybe [Text])
    , _vcVPCId            :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'VPCConfigResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vcSecurityGroupIds'
--
-- * 'vcSubnetIds'
--
-- * 'vcVPCId'
vpcConfigResponse
    :: VPCConfigResponse
vpcConfigResponse =
    VPCConfigResponse'
    { _vcSecurityGroupIds = Nothing
    , _vcSubnetIds = Nothing
    , _vcVPCId = Nothing
    }

-- | A list of security group IDs associated with the Lambda function.
vcSecurityGroupIds :: Lens' VPCConfigResponse [Text]
vcSecurityGroupIds = lens _vcSecurityGroupIds (\ s a -> s{_vcSecurityGroupIds = a}) . _Default . _Coerce;

-- | A list of subnet IDs associated with the Lambda function.
vcSubnetIds :: Lens' VPCConfigResponse [Text]
vcSubnetIds = lens _vcSubnetIds (\ s a -> s{_vcSubnetIds = a}) . _Default . _Coerce;

-- | The VPC ID associated with you Lambda function.
vcVPCId :: Lens' VPCConfigResponse (Maybe Text)
vcVPCId = lens _vcVPCId (\ s a -> s{_vcVPCId = a});

instance FromJSON VPCConfigResponse where
        parseJSON
          = withObject "VPCConfigResponse"
              (\ x ->
                 VPCConfigResponse' <$>
                   (x .:? "SecurityGroupIds" .!= mempty) <*>
                     (x .:? "SubnetIds" .!= mempty)
                     <*> (x .:? "VpcId"))

instance Hashable VPCConfigResponse

instance NFData VPCConfigResponse
