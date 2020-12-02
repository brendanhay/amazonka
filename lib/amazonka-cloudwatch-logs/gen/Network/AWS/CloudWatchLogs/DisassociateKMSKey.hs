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
-- Module      : Network.AWS.CloudWatchLogs.DisassociateKMSKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates the associated AWS Key Management Service (AWS KMS) customer master key (CMK) from the specified log group.
--
--
-- After the AWS KMS CMK is disassociated from the log group, AWS CloudWatch Logs stops encrypting newly ingested data for the log group. All previously ingested data remains encrypted, and AWS CloudWatch Logs requires permissions for the CMK whenever the encrypted data is requested.
--
-- Note that it can take up to 5 minutes for this operation to take effect.
--
module Network.AWS.CloudWatchLogs.DisassociateKMSKey
    (
    -- * Creating a Request
      disassociateKMSKey
    , DisassociateKMSKey
    -- * Request Lenses
    , dkkLogGroupName

    -- * Destructuring the Response
    , disassociateKMSKeyResponse
    , DisassociateKMSKeyResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateKMSKey' smart constructor.
newtype DisassociateKMSKey = DisassociateKMSKey'
  { _dkkLogGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateKMSKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dkkLogGroupName' - The name of the log group.
disassociateKMSKey
    :: Text -- ^ 'dkkLogGroupName'
    -> DisassociateKMSKey
disassociateKMSKey pLogGroupName_ =
  DisassociateKMSKey' {_dkkLogGroupName = pLogGroupName_}


-- | The name of the log group.
dkkLogGroupName :: Lens' DisassociateKMSKey Text
dkkLogGroupName = lens _dkkLogGroupName (\ s a -> s{_dkkLogGroupName = a})

instance AWSRequest DisassociateKMSKey where
        type Rs DisassociateKMSKey =
             DisassociateKMSKeyResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DisassociateKMSKeyResponse'

instance Hashable DisassociateKMSKey where

instance NFData DisassociateKMSKey where

instance ToHeaders DisassociateKMSKey where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DisassociateKmsKey" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DisassociateKMSKey where
        toJSON DisassociateKMSKey'{..}
          = object
              (catMaybes
                 [Just ("logGroupName" .= _dkkLogGroupName)])

instance ToPath DisassociateKMSKey where
        toPath = const "/"

instance ToQuery DisassociateKMSKey where
        toQuery = const mempty

-- | /See:/ 'disassociateKMSKeyResponse' smart constructor.
data DisassociateKMSKeyResponse =
  DisassociateKMSKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DisassociateKMSKeyResponse' with the minimum fields required to make a request.
--
disassociateKMSKeyResponse
    :: DisassociateKMSKeyResponse
disassociateKMSKeyResponse = DisassociateKMSKeyResponse'


instance NFData DisassociateKMSKeyResponse where
