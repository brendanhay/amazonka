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
-- Module      : Network.AWS.CloudWatchLogs.AssociateKMSKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates the specified AWS Key Management Service (AWS KMS) customer master key (CMK) with the specified log group.
--
--
-- Associating an AWS KMS CMK with a log group overrides any existing associations between the log group and a CMK. After a CMK is associated with a log group, all newly ingested data for the log group is encrypted using the CMK. This association is stored as long as the data encrypted with the CMK is still within Amazon CloudWatch Logs. This enables Amazon CloudWatch Logs to decrypt this data whenever it is requested.
--
-- Note that it can take up to 5 minutes for this operation to take effect.
--
-- If you attempt to associate a CMK with a log group but the CMK does not exist or the CMK is disabled, you will receive an @InvalidParameterException@ error.
--
module Network.AWS.CloudWatchLogs.AssociateKMSKey
    (
    -- * Creating a Request
      associateKMSKey
    , AssociateKMSKey
    -- * Request Lenses
    , akkLogGroupName
    , akkKmsKeyId

    -- * Destructuring the Response
    , associateKMSKeyResponse
    , AssociateKMSKeyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'associateKMSKey' smart constructor.
data AssociateKMSKey = AssociateKMSKey'
  { _akkLogGroupName :: !Text
  , _akkKmsKeyId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateKMSKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akkLogGroupName' - The name of the log group.
--
-- * 'akkKmsKeyId' - The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
associateKMSKey
    :: Text -- ^ 'akkLogGroupName'
    -> Text -- ^ 'akkKmsKeyId'
    -> AssociateKMSKey
associateKMSKey pLogGroupName_ pKmsKeyId_ =
  AssociateKMSKey'
    {_akkLogGroupName = pLogGroupName_, _akkKmsKeyId = pKmsKeyId_}


-- | The name of the log group.
akkLogGroupName :: Lens' AssociateKMSKey Text
akkLogGroupName = lens _akkLogGroupName (\ s a -> s{_akkLogGroupName = a})

-- | The Amazon Resource Name (ARN) of the CMK to use when encrypting log data. For more information, see <http://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html#arn-syntax-kms Amazon Resource Names - AWS Key Management Service (AWS KMS)> .
akkKmsKeyId :: Lens' AssociateKMSKey Text
akkKmsKeyId = lens _akkKmsKeyId (\ s a -> s{_akkKmsKeyId = a})

instance AWSRequest AssociateKMSKey where
        type Rs AssociateKMSKey = AssociateKMSKeyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull AssociateKMSKeyResponse'

instance Hashable AssociateKMSKey where

instance NFData AssociateKMSKey where

instance ToHeaders AssociateKMSKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.AssociateKmsKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AssociateKMSKey where
        toJSON AssociateKMSKey'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _akkLogGroupName),
                  Just ("kmsKeyId" .= _akkKmsKeyId)])

instance ToPath AssociateKMSKey where
        toPath = const "/"

instance ToQuery AssociateKMSKey where
        toQuery = const mempty

-- | /See:/ 'associateKMSKeyResponse' smart constructor.
data AssociateKMSKeyResponse =
  AssociateKMSKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AssociateKMSKeyResponse' with the minimum fields required to make a request.
--
associateKMSKeyResponse
    :: AssociateKMSKeyResponse
associateKMSKeyResponse = AssociateKMSKeyResponse'


instance NFData AssociateKMSKeyResponse where
