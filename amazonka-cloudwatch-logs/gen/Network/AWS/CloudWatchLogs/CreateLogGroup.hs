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
-- Module      : Network.AWS.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a log group with the specified name.
--
--
-- You can create up to 5000 log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
--     * Log group names must be unique within a region for an AWS account.
--
--     * Log group names can be between 1 and 512 characters long.
--
--     * Log group names consist of the following characters: a-z, A-Z, 0-9, '_' (underscore), '-' (hyphen), '/' (forward slash), and '.' (period).
--
--
--
-- If you associate a AWS Key Management Service (AWS KMS) customer master key (CMK) with the log group, ingested data is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
--
-- If you attempt to associate a CMK with the log group but the CMK does not exist or the CMK is disabled, you will receive an @InvalidParameterException@ error.
--
module Network.AWS.CloudWatchLogs.CreateLogGroup
    (
    -- * Creating a Request
      createLogGroup
    , CreateLogGroup
    -- * Request Lenses
    , clgKmsKeyId
    , clgTags
    , clgLogGroupName

    -- * Destructuring the Response
    , createLogGroupResponse
    , CreateLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createLogGroup' smart constructor.
data CreateLogGroup = CreateLogGroup'
  { _clgKmsKeyId     :: !(Maybe Text)
  , _clgTags         :: !(Maybe (Map Text Text))
  , _clgLogGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgKmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
--
-- * 'clgTags' - The key-value pairs to use for the tags.
--
-- * 'clgLogGroupName' - The name of the log group.
createLogGroup
    :: Text -- ^ 'clgLogGroupName'
    -> CreateLogGroup
createLogGroup pLogGroupName_ =
  CreateLogGroup'
    { _clgKmsKeyId = Nothing
    , _clgTags = Nothing
    , _clgLogGroupName = pLogGroupName_
    }


-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
clgKmsKeyId :: Lens' CreateLogGroup (Maybe Text)
clgKmsKeyId = lens _clgKmsKeyId (\ s a -> s{_clgKmsKeyId = a})

-- | The key-value pairs to use for the tags.
clgTags :: Lens' CreateLogGroup (HashMap Text Text)
clgTags = lens _clgTags (\ s a -> s{_clgTags = a}) . _Default . _Map

-- | The name of the log group.
clgLogGroupName :: Lens' CreateLogGroup Text
clgLogGroupName = lens _clgLogGroupName (\ s a -> s{_clgLogGroupName = a})

instance AWSRequest CreateLogGroup where
        type Rs CreateLogGroup = CreateLogGroupResponse
        request = postJSON cloudWatchLogs
        response = receiveNull CreateLogGroupResponse'

instance Hashable CreateLogGroup where

instance NFData CreateLogGroup where

instance ToHeaders CreateLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.CreateLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLogGroup where
        toJSON CreateLogGroup'{..}
          = object
              (catMaybes
                 [("kmsKeyId" .=) <$> _clgKmsKeyId,
                  ("tags" .=) <$> _clgTags,
                  Just ("logGroupName" .= _clgLogGroupName)])

instance ToPath CreateLogGroup where
        toPath = const "/"

instance ToQuery CreateLogGroup where
        toQuery = const mempty

-- | /See:/ 'createLogGroupResponse' smart constructor.
data CreateLogGroupResponse =
  CreateLogGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateLogGroupResponse' with the minimum fields required to make a request.
--
createLogGroupResponse
    :: CreateLogGroupResponse
createLogGroupResponse = CreateLogGroupResponse'


instance NFData CreateLogGroupResponse where
