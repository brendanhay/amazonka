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
-- Module      : Network.AWS.Batch.RegisterJobDefinition
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers an AWS Batch job definition.
--
--
module Network.AWS.Batch.RegisterJobDefinition
    (
    -- * Creating a Request
      registerJobDefinition
    , RegisterJobDefinition
    -- * Request Lenses
    , rjdRetryStrategy
    , rjdParameters
    , rjdTimeout
    , rjdContainerProperties
    , rjdJobDefinitionName
    , rjdType

    -- * Destructuring the Response
    , registerJobDefinitionResponse
    , RegisterJobDefinitionResponse
    -- * Response Lenses
    , rjdrsResponseStatus
    , rjdrsJobDefinitionName
    , rjdrsJobDefinitionARN
    , rjdrsRevision
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerJobDefinition' smart constructor.
data RegisterJobDefinition = RegisterJobDefinition'
  { _rjdRetryStrategy       :: !(Maybe RetryStrategy)
  , _rjdParameters          :: !(Maybe (Map Text Text))
  , _rjdTimeout             :: !(Maybe JobTimeout)
  , _rjdContainerProperties :: !(Maybe ContainerProperties)
  , _rjdJobDefinitionName   :: !Text
  , _rjdType                :: !JobDefinitionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterJobDefinition' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjdRetryStrategy' - The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
--
-- * 'rjdParameters' - Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
--
-- * 'rjdTimeout' - The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- * 'rjdContainerProperties' - An object with various properties specific for container-based jobs. This parameter is required if the @type@ parameter is @container@ .
--
-- * 'rjdJobDefinitionName' - The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
--
-- * 'rjdType' - The type of job definition.
registerJobDefinition
    :: Text -- ^ 'rjdJobDefinitionName'
    -> JobDefinitionType -- ^ 'rjdType'
    -> RegisterJobDefinition
registerJobDefinition pJobDefinitionName_ pType_ =
  RegisterJobDefinition'
    { _rjdRetryStrategy = Nothing
    , _rjdParameters = Nothing
    , _rjdTimeout = Nothing
    , _rjdContainerProperties = Nothing
    , _rjdJobDefinitionName = pJobDefinitionName_
    , _rjdType = pType_
    }


-- | The retry strategy to use for failed jobs that are submitted with this job definition. Any retry strategy that is specified during a 'SubmitJob' operation overrides the retry strategy defined here. If a job is terminated due to a timeout, it is not retried.
rjdRetryStrategy :: Lens' RegisterJobDefinition (Maybe RetryStrategy)
rjdRetryStrategy = lens _rjdRetryStrategy (\ s a -> s{_rjdRetryStrategy = a})

-- | Default parameter substitution placeholders to set in the job definition. Parameters are specified as a key-value pair mapping. Parameters in a @SubmitJob@ request override any corresponding parameter defaults from the job definition.
rjdParameters :: Lens' RegisterJobDefinition (HashMap Text Text)
rjdParameters = lens _rjdParameters (\ s a -> s{_rjdParameters = a}) . _Default . _Map

-- | The timeout configuration for jobs that are submitted with this job definition, after which AWS Batch terminates your jobs if they have not finished. If a job is terminated due to a timeout, it is not retried. The minimum value for the timeout is 60 seconds. Any timeout configuration that is specified during a 'SubmitJob' operation overrides the timeout configuration defined here. For more information, see <http://docs.aws.amazon.com/AmazonECS/latest/developerguide/job_timeouts.html Job Timeouts> in the /Amazon Elastic Container Service Developer Guide/ .
rjdTimeout :: Lens' RegisterJobDefinition (Maybe JobTimeout)
rjdTimeout = lens _rjdTimeout (\ s a -> s{_rjdTimeout = a})

-- | An object with various properties specific for container-based jobs. This parameter is required if the @type@ parameter is @container@ .
rjdContainerProperties :: Lens' RegisterJobDefinition (Maybe ContainerProperties)
rjdContainerProperties = lens _rjdContainerProperties (\ s a -> s{_rjdContainerProperties = a})

-- | The name of the job definition to register. Up to 128 letters (uppercase and lowercase), numbers, hyphens, and underscores are allowed.
rjdJobDefinitionName :: Lens' RegisterJobDefinition Text
rjdJobDefinitionName = lens _rjdJobDefinitionName (\ s a -> s{_rjdJobDefinitionName = a})

-- | The type of job definition.
rjdType :: Lens' RegisterJobDefinition JobDefinitionType
rjdType = lens _rjdType (\ s a -> s{_rjdType = a})

instance AWSRequest RegisterJobDefinition where
        type Rs RegisterJobDefinition =
             RegisterJobDefinitionResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 RegisterJobDefinitionResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "jobDefinitionName")
                     <*> (x .:> "jobDefinitionArn")
                     <*> (x .:> "revision"))

instance Hashable RegisterJobDefinition where

instance NFData RegisterJobDefinition where

instance ToHeaders RegisterJobDefinition where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON RegisterJobDefinition where
        toJSON RegisterJobDefinition'{..}
          = object
              (catMaybes
                 [("retryStrategy" .=) <$> _rjdRetryStrategy,
                  ("parameters" .=) <$> _rjdParameters,
                  ("timeout" .=) <$> _rjdTimeout,
                  ("containerProperties" .=) <$>
                    _rjdContainerProperties,
                  Just ("jobDefinitionName" .= _rjdJobDefinitionName),
                  Just ("type" .= _rjdType)])

instance ToPath RegisterJobDefinition where
        toPath = const "/v1/registerjobdefinition"

instance ToQuery RegisterJobDefinition where
        toQuery = const mempty

-- | /See:/ 'registerJobDefinitionResponse' smart constructor.
data RegisterJobDefinitionResponse = RegisterJobDefinitionResponse'
  { _rjdrsResponseStatus    :: !Int
  , _rjdrsJobDefinitionName :: !Text
  , _rjdrsJobDefinitionARN  :: !Text
  , _rjdrsRevision          :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RegisterJobDefinitionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rjdrsResponseStatus' - -- | The response status code.
--
-- * 'rjdrsJobDefinitionName' - The name of the job definition.
--
-- * 'rjdrsJobDefinitionARN' - The Amazon Resource Name (ARN) of the job definition.
--
-- * 'rjdrsRevision' - The revision of the job definition.
registerJobDefinitionResponse
    :: Int -- ^ 'rjdrsResponseStatus'
    -> Text -- ^ 'rjdrsJobDefinitionName'
    -> Text -- ^ 'rjdrsJobDefinitionARN'
    -> Int -- ^ 'rjdrsRevision'
    -> RegisterJobDefinitionResponse
registerJobDefinitionResponse pResponseStatus_ pJobDefinitionName_ pJobDefinitionARN_ pRevision_ =
  RegisterJobDefinitionResponse'
    { _rjdrsResponseStatus = pResponseStatus_
    , _rjdrsJobDefinitionName = pJobDefinitionName_
    , _rjdrsJobDefinitionARN = pJobDefinitionARN_
    , _rjdrsRevision = pRevision_
    }


-- | -- | The response status code.
rjdrsResponseStatus :: Lens' RegisterJobDefinitionResponse Int
rjdrsResponseStatus = lens _rjdrsResponseStatus (\ s a -> s{_rjdrsResponseStatus = a})

-- | The name of the job definition.
rjdrsJobDefinitionName :: Lens' RegisterJobDefinitionResponse Text
rjdrsJobDefinitionName = lens _rjdrsJobDefinitionName (\ s a -> s{_rjdrsJobDefinitionName = a})

-- | The Amazon Resource Name (ARN) of the job definition.
rjdrsJobDefinitionARN :: Lens' RegisterJobDefinitionResponse Text
rjdrsJobDefinitionARN = lens _rjdrsJobDefinitionARN (\ s a -> s{_rjdrsJobDefinitionARN = a})

-- | The revision of the job definition.
rjdrsRevision :: Lens' RegisterJobDefinitionResponse Int
rjdrsRevision = lens _rjdrsRevision (\ s a -> s{_rjdrsRevision = a})

instance NFData RegisterJobDefinitionResponse where
