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
-- Module      : Network.AWS.Cloud9.CreateEnvironmentEC
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS Cloud9 development environment, launches an Amazon Elastic Compute Cloud (Amazon EC2) instance, and then connects from the instance to the environment.
--
--
module Network.AWS.Cloud9.CreateEnvironmentEC
    (
    -- * Creating a Request
      createEnvironmentEC
    , CreateEnvironmentEC
    -- * Request Lenses
    , ceecAutomaticStopTimeMinutes
    , ceecSubnetId
    , ceecOwnerARN
    , ceecClientRequestToken
    , ceecDescription
    , ceecName
    , ceecInstanceType

    -- * Destructuring the Response
    , createEnvironmentECResponse
    , CreateEnvironmentECResponse
    -- * Response Lenses
    , ceecrsEnvironmentId
    , ceecrsResponseStatus
    ) where

import Network.AWS.Cloud9.Types
import Network.AWS.Cloud9.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createEnvironmentEC' smart constructor.
data CreateEnvironmentEC = CreateEnvironmentEC'
  { _ceecAutomaticStopTimeMinutes :: !(Maybe Int)
  , _ceecSubnetId                 :: !(Maybe Text)
  , _ceecOwnerARN                 :: !(Maybe Text)
  , _ceecClientRequestToken       :: !(Maybe Text)
  , _ceecDescription              :: !(Maybe Text)
  , _ceecName                     :: !Text
  , _ceecInstanceType             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEnvironmentEC' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceecAutomaticStopTimeMinutes' - The number of minutes until the running instance is shut down after the environment has last been used.
--
-- * 'ceecSubnetId' - The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
--
-- * 'ceecOwnerARN' - The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
--
-- * 'ceecClientRequestToken' - A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
--
-- * 'ceecDescription' - The description of the environment to create.
--
-- * 'ceecName' - The name of the environment to create. This name is visible to other AWS IAM users in the same AWS account.
--
-- * 'ceecInstanceType' - The type of instance to connect to the environment (for example, @t2.micro@ ).
createEnvironmentEC
    :: Text -- ^ 'ceecName'
    -> Text -- ^ 'ceecInstanceType'
    -> CreateEnvironmentEC
createEnvironmentEC pName_ pInstanceType_ =
  CreateEnvironmentEC'
    { _ceecAutomaticStopTimeMinutes = Nothing
    , _ceecSubnetId = Nothing
    , _ceecOwnerARN = Nothing
    , _ceecClientRequestToken = Nothing
    , _ceecDescription = Nothing
    , _ceecName = pName_
    , _ceecInstanceType = pInstanceType_
    }


-- | The number of minutes until the running instance is shut down after the environment has last been used.
ceecAutomaticStopTimeMinutes :: Lens' CreateEnvironmentEC (Maybe Int)
ceecAutomaticStopTimeMinutes = lens _ceecAutomaticStopTimeMinutes (\ s a -> s{_ceecAutomaticStopTimeMinutes = a})

-- | The ID of the subnet in Amazon VPC that AWS Cloud9 will use to communicate with the Amazon EC2 instance.
ceecSubnetId :: Lens' CreateEnvironmentEC (Maybe Text)
ceecSubnetId = lens _ceecSubnetId (\ s a -> s{_ceecSubnetId = a})

-- | The Amazon Resource Name (ARN) of the environment owner. This ARN can be the ARN of any AWS IAM principal. If this value is not specified, the ARN defaults to this environment's creator.
ceecOwnerARN :: Lens' CreateEnvironmentEC (Maybe Text)
ceecOwnerARN = lens _ceecOwnerARN (\ s a -> s{_ceecOwnerARN = a})

-- | A unique, case-sensitive string that helps AWS Cloud9 to ensure this operation completes no more than one time. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Client Tokens> in the /Amazon EC2 API Reference/ .
ceecClientRequestToken :: Lens' CreateEnvironmentEC (Maybe Text)
ceecClientRequestToken = lens _ceecClientRequestToken (\ s a -> s{_ceecClientRequestToken = a})

-- | The description of the environment to create.
ceecDescription :: Lens' CreateEnvironmentEC (Maybe Text)
ceecDescription = lens _ceecDescription (\ s a -> s{_ceecDescription = a})

-- | The name of the environment to create. This name is visible to other AWS IAM users in the same AWS account.
ceecName :: Lens' CreateEnvironmentEC Text
ceecName = lens _ceecName (\ s a -> s{_ceecName = a})

-- | The type of instance to connect to the environment (for example, @t2.micro@ ).
ceecInstanceType :: Lens' CreateEnvironmentEC Text
ceecInstanceType = lens _ceecInstanceType (\ s a -> s{_ceecInstanceType = a})

instance AWSRequest CreateEnvironmentEC where
        type Rs CreateEnvironmentEC =
             CreateEnvironmentECResponse
        request = postJSON cloud9
        response
          = receiveJSON
              (\ s h x ->
                 CreateEnvironmentECResponse' <$>
                   (x .?> "environmentId") <*> (pure (fromEnum s)))

instance Hashable CreateEnvironmentEC where

instance NFData CreateEnvironmentEC where

instance ToHeaders CreateEnvironmentEC where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCloud9WorkspaceManagementService.CreateEnvironmentEC"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateEnvironmentEC where
        toJSON CreateEnvironmentEC'{..}
          = object
              (catMaybes
                 [("automaticStopTimeMinutes" .=) <$>
                    _ceecAutomaticStopTimeMinutes,
                  ("subnetId" .=) <$> _ceecSubnetId,
                  ("ownerArn" .=) <$> _ceecOwnerARN,
                  ("clientRequestToken" .=) <$>
                    _ceecClientRequestToken,
                  ("description" .=) <$> _ceecDescription,
                  Just ("name" .= _ceecName),
                  Just ("instanceType" .= _ceecInstanceType)])

instance ToPath CreateEnvironmentEC where
        toPath = const "/"

instance ToQuery CreateEnvironmentEC where
        toQuery = const mempty

-- | /See:/ 'createEnvironmentECResponse' smart constructor.
data CreateEnvironmentECResponse = CreateEnvironmentECResponse'
  { _ceecrsEnvironmentId  :: !(Maybe Text)
  , _ceecrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateEnvironmentECResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ceecrsEnvironmentId' - The ID of the environment that was created.
--
-- * 'ceecrsResponseStatus' - -- | The response status code.
createEnvironmentECResponse
    :: Int -- ^ 'ceecrsResponseStatus'
    -> CreateEnvironmentECResponse
createEnvironmentECResponse pResponseStatus_ =
  CreateEnvironmentECResponse'
    {_ceecrsEnvironmentId = Nothing, _ceecrsResponseStatus = pResponseStatus_}


-- | The ID of the environment that was created.
ceecrsEnvironmentId :: Lens' CreateEnvironmentECResponse (Maybe Text)
ceecrsEnvironmentId = lens _ceecrsEnvironmentId (\ s a -> s{_ceecrsEnvironmentId = a})

-- | -- | The response status code.
ceecrsResponseStatus :: Lens' CreateEnvironmentECResponse Int
ceecrsResponseStatus = lens _ceecrsResponseStatus (\ s a -> s{_ceecrsResponseStatus = a})

instance NFData CreateEnvironmentECResponse where
