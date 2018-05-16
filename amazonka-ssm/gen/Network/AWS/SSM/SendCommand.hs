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
-- Module      : Network.AWS.SSM.SendCommand
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Executes commands on one or more managed instances.
--
--
module Network.AWS.SSM.SendCommand
    (
    -- * Creating a Request
      sendCommand
    , SendCommand
    -- * Request Lenses
    , scServiceRoleARN
    , scNotificationConfig
    , scDocumentHashType
    , scOutputS3KeyPrefix
    , scMaxErrors
    , scInstanceIds
    , scOutputS3Region
    , scTargets
    , scParameters
    , scDocumentHash
    , scDocumentVersion
    , scTimeoutSeconds
    , scComment
    , scOutputS3BucketName
    , scMaxConcurrency
    , scDocumentName

    -- * Destructuring the Response
    , sendCommandResponse
    , SendCommandResponse
    -- * Response Lenses
    , scrsCommand
    , scrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'sendCommand' smart constructor.
data SendCommand = SendCommand'
  { _scServiceRoleARN     :: !(Maybe Text)
  , _scNotificationConfig :: !(Maybe NotificationConfig)
  , _scDocumentHashType   :: !(Maybe DocumentHashType)
  , _scOutputS3KeyPrefix  :: !(Maybe Text)
  , _scMaxErrors          :: !(Maybe Text)
  , _scInstanceIds        :: !(Maybe [Text])
  , _scOutputS3Region     :: !(Maybe Text)
  , _scTargets            :: !(Maybe [Target])
  , _scParameters         :: !(Maybe (Map Text [Text]))
  , _scDocumentHash       :: !(Maybe Text)
  , _scDocumentVersion    :: !(Maybe Text)
  , _scTimeoutSeconds     :: !(Maybe Nat)
  , _scComment            :: !(Maybe Text)
  , _scOutputS3BucketName :: !(Maybe Text)
  , _scMaxConcurrency     :: !(Maybe Text)
  , _scDocumentName       :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendCommand' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scServiceRoleARN' - The IAM role that Systems Manager uses to send notifications.
--
-- * 'scNotificationConfig' - Configurations for sending notifications.
--
-- * 'scDocumentHashType' - Sha256 or Sha1.
--
-- * 'scOutputS3KeyPrefix' - The directory structure within the S3 bucket where the responses should be stored.
--
-- * 'scMaxErrors' - The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-maxerrors.html Using Error Controls> .
--
-- * 'scInstanceIds' - The instance IDs where the command should execute. You can specify a maximum of 50 IDs. If you prefer not to list individual instance IDs, you can instead send commands to a fleet of instances using the Targets parameter, which accepts EC2 tags. For more information about how to use Targets, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending Commands to a Fleet> .
--
-- * 'scOutputS3Region' - (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
--
-- * 'scTargets' - (Optional) An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call. For more information about how to use Targets, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending Commands to a Fleet> .
--
-- * 'scParameters' - The required and optional parameters specified in the document being executed.
--
-- * 'scDocumentHash' - The Sha256 or Sha1 hash created by the system when the document was created.
--
-- * 'scDocumentVersion' - The SSM document version to use in the request. You can specify Default, Latest, or a specific version number.
--
-- * 'scTimeoutSeconds' - If this time is reached and the command has not already started executing, it will not run.
--
-- * 'scComment' - User-specified information about the command, such as a brief description of what the command should do.
--
-- * 'scOutputS3BucketName' - The name of the S3 bucket where command execution responses should be stored.
--
-- * 'scMaxConcurrency' - (Optional) The maximum number of instances that are allowed to execute the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-velocity.html Using Concurrency Controls> .
--
-- * 'scDocumentName' - Required. The name of the Systems Manager document to execute. This can be a public document or a custom document.
sendCommand
    :: Text -- ^ 'scDocumentName'
    -> SendCommand
sendCommand pDocumentName_ =
  SendCommand'
    { _scServiceRoleARN = Nothing
    , _scNotificationConfig = Nothing
    , _scDocumentHashType = Nothing
    , _scOutputS3KeyPrefix = Nothing
    , _scMaxErrors = Nothing
    , _scInstanceIds = Nothing
    , _scOutputS3Region = Nothing
    , _scTargets = Nothing
    , _scParameters = Nothing
    , _scDocumentHash = Nothing
    , _scDocumentVersion = Nothing
    , _scTimeoutSeconds = Nothing
    , _scComment = Nothing
    , _scOutputS3BucketName = Nothing
    , _scMaxConcurrency = Nothing
    , _scDocumentName = pDocumentName_
    }


-- | The IAM role that Systems Manager uses to send notifications.
scServiceRoleARN :: Lens' SendCommand (Maybe Text)
scServiceRoleARN = lens _scServiceRoleARN (\ s a -> s{_scServiceRoleARN = a})

-- | Configurations for sending notifications.
scNotificationConfig :: Lens' SendCommand (Maybe NotificationConfig)
scNotificationConfig = lens _scNotificationConfig (\ s a -> s{_scNotificationConfig = a})

-- | Sha256 or Sha1.
scDocumentHashType :: Lens' SendCommand (Maybe DocumentHashType)
scDocumentHashType = lens _scDocumentHashType (\ s a -> s{_scDocumentHashType = a})

-- | The directory structure within the S3 bucket where the responses should be stored.
scOutputS3KeyPrefix :: Lens' SendCommand (Maybe Text)
scOutputS3KeyPrefix = lens _scOutputS3KeyPrefix (\ s a -> s{_scOutputS3KeyPrefix = a})

-- | The maximum number of errors allowed without the command failing. When the command fails one more time beyond the value of MaxErrors, the systems stops sending the command to additional targets. You can specify a number like 10 or a percentage like 10%. The default value is 0. For more information about how to use MaxErrors, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-maxerrors.html Using Error Controls> .
scMaxErrors :: Lens' SendCommand (Maybe Text)
scMaxErrors = lens _scMaxErrors (\ s a -> s{_scMaxErrors = a})

-- | The instance IDs where the command should execute. You can specify a maximum of 50 IDs. If you prefer not to list individual instance IDs, you can instead send commands to a fleet of instances using the Targets parameter, which accepts EC2 tags. For more information about how to use Targets, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending Commands to a Fleet> .
scInstanceIds :: Lens' SendCommand [Text]
scInstanceIds = lens _scInstanceIds (\ s a -> s{_scInstanceIds = a}) . _Default . _Coerce

-- | (Deprecated) You can no longer specify this parameter. The system ignores it. Instead, Systems Manager automatically determines the Amazon S3 bucket region.
scOutputS3Region :: Lens' SendCommand (Maybe Text)
scOutputS3Region = lens _scOutputS3Region (\ s a -> s{_scOutputS3Region = a})

-- | (Optional) An array of search criteria that targets instances using a Key,Value combination that you specify. Targets is required if you don't provide one or more instance IDs in the call. For more information about how to use Targets, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-multiple.html Sending Commands to a Fleet> .
scTargets :: Lens' SendCommand [Target]
scTargets = lens _scTargets (\ s a -> s{_scTargets = a}) . _Default . _Coerce

-- | The required and optional parameters specified in the document being executed.
scParameters :: Lens' SendCommand (HashMap Text [Text])
scParameters = lens _scParameters (\ s a -> s{_scParameters = a}) . _Default . _Map

-- | The Sha256 or Sha1 hash created by the system when the document was created.
scDocumentHash :: Lens' SendCommand (Maybe Text)
scDocumentHash = lens _scDocumentHash (\ s a -> s{_scDocumentHash = a})

-- | The SSM document version to use in the request. You can specify Default, Latest, or a specific version number.
scDocumentVersion :: Lens' SendCommand (Maybe Text)
scDocumentVersion = lens _scDocumentVersion (\ s a -> s{_scDocumentVersion = a})

-- | If this time is reached and the command has not already started executing, it will not run.
scTimeoutSeconds :: Lens' SendCommand (Maybe Natural)
scTimeoutSeconds = lens _scTimeoutSeconds (\ s a -> s{_scTimeoutSeconds = a}) . mapping _Nat

-- | User-specified information about the command, such as a brief description of what the command should do.
scComment :: Lens' SendCommand (Maybe Text)
scComment = lens _scComment (\ s a -> s{_scComment = a})

-- | The name of the S3 bucket where command execution responses should be stored.
scOutputS3BucketName :: Lens' SendCommand (Maybe Text)
scOutputS3BucketName = lens _scOutputS3BucketName (\ s a -> s{_scOutputS3BucketName = a})

-- | (Optional) The maximum number of instances that are allowed to execute the command at the same time. You can specify a number such as 10 or a percentage such as 10%. The default value is 50. For more information about how to use MaxConcurrency, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/send-commands-velocity.html Using Concurrency Controls> .
scMaxConcurrency :: Lens' SendCommand (Maybe Text)
scMaxConcurrency = lens _scMaxConcurrency (\ s a -> s{_scMaxConcurrency = a})

-- | Required. The name of the Systems Manager document to execute. This can be a public document or a custom document.
scDocumentName :: Lens' SendCommand Text
scDocumentName = lens _scDocumentName (\ s a -> s{_scDocumentName = a})

instance AWSRequest SendCommand where
        type Rs SendCommand = SendCommandResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 SendCommandResponse' <$>
                   (x .?> "Command") <*> (pure (fromEnum s)))

instance Hashable SendCommand where

instance NFData SendCommand where

instance ToHeaders SendCommand where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.SendCommand" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SendCommand where
        toJSON SendCommand'{..}
          = object
              (catMaybes
                 [("ServiceRoleArn" .=) <$> _scServiceRoleARN,
                  ("NotificationConfig" .=) <$> _scNotificationConfig,
                  ("DocumentHashType" .=) <$> _scDocumentHashType,
                  ("OutputS3KeyPrefix" .=) <$> _scOutputS3KeyPrefix,
                  ("MaxErrors" .=) <$> _scMaxErrors,
                  ("InstanceIds" .=) <$> _scInstanceIds,
                  ("OutputS3Region" .=) <$> _scOutputS3Region,
                  ("Targets" .=) <$> _scTargets,
                  ("Parameters" .=) <$> _scParameters,
                  ("DocumentHash" .=) <$> _scDocumentHash,
                  ("DocumentVersion" .=) <$> _scDocumentVersion,
                  ("TimeoutSeconds" .=) <$> _scTimeoutSeconds,
                  ("Comment" .=) <$> _scComment,
                  ("OutputS3BucketName" .=) <$> _scOutputS3BucketName,
                  ("MaxConcurrency" .=) <$> _scMaxConcurrency,
                  Just ("DocumentName" .= _scDocumentName)])

instance ToPath SendCommand where
        toPath = const "/"

instance ToQuery SendCommand where
        toQuery = const mempty

-- | /See:/ 'sendCommandResponse' smart constructor.
data SendCommandResponse = SendCommandResponse'
  { _scrsCommand        :: !(Maybe Command)
  , _scrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SendCommandResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scrsCommand' - The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
--
-- * 'scrsResponseStatus' - -- | The response status code.
sendCommandResponse
    :: Int -- ^ 'scrsResponseStatus'
    -> SendCommandResponse
sendCommandResponse pResponseStatus_ =
  SendCommandResponse'
    {_scrsCommand = Nothing, _scrsResponseStatus = pResponseStatus_}


-- | The request as it was received by Systems Manager. Also provides the command ID which can be used future references to this request.
scrsCommand :: Lens' SendCommandResponse (Maybe Command)
scrsCommand = lens _scrsCommand (\ s a -> s{_scrsCommand = a})

-- | -- | The response status code.
scrsResponseStatus :: Lens' SendCommandResponse Int
scrsResponseStatus = lens _scrsResponseStatus (\ s a -> s{_scrsResponseStatus = a})

instance NFData SendCommandResponse where
