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
-- Module      : Network.AWS.SageMaker.DescribeModel
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a model that you created using the @CreateModel@ API.
--
--
module Network.AWS.SageMaker.DescribeModel
    (
    -- * Creating a Request
      describeModel
    , DescribeModel
    -- * Request Lenses
    , dModelName

    -- * Destructuring the Response
    , describeModelResponse
    , DescribeModelResponse
    -- * Response Lenses
    , dmrsVPCConfig
    , dmrsResponseStatus
    , dmrsModelName
    , dmrsPrimaryContainer
    , dmrsExecutionRoleARN
    , dmrsCreationTime
    , dmrsModelARN
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SageMaker.Types
import Network.AWS.SageMaker.Types.Product

-- | /See:/ 'describeModel' smart constructor.
newtype DescribeModel = DescribeModel'
  { _dModelName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeModel' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dModelName' - The name of the model.
describeModel
    :: Text -- ^ 'dModelName'
    -> DescribeModel
describeModel pModelName_ = DescribeModel' {_dModelName = pModelName_}


-- | The name of the model.
dModelName :: Lens' DescribeModel Text
dModelName = lens _dModelName (\ s a -> s{_dModelName = a})

instance AWSRequest DescribeModel where
        type Rs DescribeModel = DescribeModelResponse
        request = postJSON sageMaker
        response
          = receiveJSON
              (\ s h x ->
                 DescribeModelResponse' <$>
                   (x .?> "VpcConfig") <*> (pure (fromEnum s)) <*>
                     (x .:> "ModelName")
                     <*> (x .:> "PrimaryContainer")
                     <*> (x .:> "ExecutionRoleArn")
                     <*> (x .:> "CreationTime")
                     <*> (x .:> "ModelArn"))

instance Hashable DescribeModel where

instance NFData DescribeModel where

instance ToHeaders DescribeModel where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("SageMaker.DescribeModel" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeModel where
        toJSON DescribeModel'{..}
          = object
              (catMaybes [Just ("ModelName" .= _dModelName)])

instance ToPath DescribeModel where
        toPath = const "/"

instance ToQuery DescribeModel where
        toQuery = const mempty

-- | /See:/ 'describeModelResponse' smart constructor.
data DescribeModelResponse = DescribeModelResponse'
  { _dmrsVPCConfig        :: !(Maybe VPCConfig)
  , _dmrsResponseStatus   :: !Int
  , _dmrsModelName        :: !Text
  , _dmrsPrimaryContainer :: !ContainerDefinition
  , _dmrsExecutionRoleARN :: !Text
  , _dmrsCreationTime     :: !POSIX
  , _dmrsModelARN         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeModelResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsVPCConfig' - A object that specifies the VPC that this model has access to. For more information, see 'host-vpc'
--
-- * 'dmrsResponseStatus' - -- | The response status code.
--
-- * 'dmrsModelName' - Name of the Amazon SageMaker model.
--
-- * 'dmrsPrimaryContainer' - The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
--
-- * 'dmrsExecutionRoleARN' - The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
--
-- * 'dmrsCreationTime' - A timestamp that shows when the model was created.
--
-- * 'dmrsModelARN' - The Amazon Resource Name (ARN) of the model.
describeModelResponse
    :: Int -- ^ 'dmrsResponseStatus'
    -> Text -- ^ 'dmrsModelName'
    -> ContainerDefinition -- ^ 'dmrsPrimaryContainer'
    -> Text -- ^ 'dmrsExecutionRoleARN'
    -> UTCTime -- ^ 'dmrsCreationTime'
    -> Text -- ^ 'dmrsModelARN'
    -> DescribeModelResponse
describeModelResponse pResponseStatus_ pModelName_ pPrimaryContainer_ pExecutionRoleARN_ pCreationTime_ pModelARN_ =
  DescribeModelResponse'
    { _dmrsVPCConfig = Nothing
    , _dmrsResponseStatus = pResponseStatus_
    , _dmrsModelName = pModelName_
    , _dmrsPrimaryContainer = pPrimaryContainer_
    , _dmrsExecutionRoleARN = pExecutionRoleARN_
    , _dmrsCreationTime = _Time # pCreationTime_
    , _dmrsModelARN = pModelARN_
    }


-- | A object that specifies the VPC that this model has access to. For more information, see 'host-vpc'
dmrsVPCConfig :: Lens' DescribeModelResponse (Maybe VPCConfig)
dmrsVPCConfig = lens _dmrsVPCConfig (\ s a -> s{_dmrsVPCConfig = a})

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DescribeModelResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\ s a -> s{_dmrsResponseStatus = a})

-- | Name of the Amazon SageMaker model.
dmrsModelName :: Lens' DescribeModelResponse Text
dmrsModelName = lens _dmrsModelName (\ s a -> s{_dmrsModelName = a})

-- | The location of the primary inference code, associated artifacts, and custom environment map that the inference code uses when it is deployed in production.
dmrsPrimaryContainer :: Lens' DescribeModelResponse ContainerDefinition
dmrsPrimaryContainer = lens _dmrsPrimaryContainer (\ s a -> s{_dmrsPrimaryContainer = a})

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for the model.
dmrsExecutionRoleARN :: Lens' DescribeModelResponse Text
dmrsExecutionRoleARN = lens _dmrsExecutionRoleARN (\ s a -> s{_dmrsExecutionRoleARN = a})

-- | A timestamp that shows when the model was created.
dmrsCreationTime :: Lens' DescribeModelResponse UTCTime
dmrsCreationTime = lens _dmrsCreationTime (\ s a -> s{_dmrsCreationTime = a}) . _Time

-- | The Amazon Resource Name (ARN) of the model.
dmrsModelARN :: Lens' DescribeModelResponse Text
dmrsModelARN = lens _dmrsModelARN (\ s a -> s{_dmrsModelARN = a})

instance NFData DescribeModelResponse where
